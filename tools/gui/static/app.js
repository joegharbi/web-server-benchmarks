/* app.js — Benchmark GUI */
'use strict';

const $ = id => document.getElementById(id);

// ─── Helpers ────────────────────────────────────────────────────────────────

function fmtBytes(b) {
  if (b == null) return '—';
  if (b < 1024) return b + ' B';
  if (b < 1048576) return (b / 1024).toFixed(1) + ' KB';
  return (b / 1048576).toFixed(1) + ' MB';
}

// ─── Theme ──────────────────────────────────────────────────────────────────

const TERM_THEME_DARK  = { background: '#0d0d0d', foreground: '#e2e8f0', cursor: '#4f8ef7', selectionBackground: '#4f8ef740' };
const TERM_THEME_LIGHT = { background: '#f0f2f5', foreground: '#1a202c', cursor: '#1a202c', selectionBackground: '#1a202c30' };

let _dark = true;
$('btn-theme').addEventListener('click', () => {
  _dark = !_dark;
  document.body.classList.toggle('light', !_dark);
  $('btn-theme').textContent = _dark ? 'Light' : 'Dark';
  term.options.theme = _dark ? TERM_THEME_DARK : TERM_THEME_LIGHT;
});

// ─── Tabs ───────────────────────────────────────────────────────────────────

document.querySelectorAll('.tab-btn').forEach(btn => {
  btn.addEventListener('click', () => {
    document.querySelectorAll('.tab-btn').forEach(b => b.classList.remove('active'));
    document.querySelectorAll('.tab-pane').forEach(p => p.classList.remove('active'));
    btn.classList.add('active');
    $('tab-' + btn.dataset.tab).classList.add('active');
    if (btn.dataset.tab === 'run')            loadRunContainers();
    if (btn.dataset.tab === 'results')        loadSessions();
    if (btn.dataset.tab === 'export')         loadExportSessions();
    if (btn.dataset.tab === 'model-selector') loadMsSessions();
  });
});

// ─── Terminal ────────────────────────────────────────────────────────────────

const term     = new Terminal({ theme: TERM_THEME_DARK, fontSize: 12, scrollback: 3000, cursorBlink: true });
const fitAddon = new FitAddon.FitAddon();
term.loadAddon(fitAddon);
term.open($('terminal'));
fitAddon.fit();

// Forward keystrokes → subprocess stdin (for sudo password prompts etc.)
term.onData(data => {
  if (_ws && _ws.readyState === WebSocket.OPEN) {
    _ws.send(JSON.stringify({ type: 'input', data }));
  }
});

// Forward terminal resize → server PTY
term.onResize(({ rows, cols }) => {
  if (_ws && _ws.readyState === WebSocket.OPEN) {
    _ws.send(JSON.stringify({ type: 'resize', rows, cols }));
  }
});

const termWrap  = $('terminal-wrap');
let _termState  = 'visible';

function setTermState(s) {
  _termState = s;
  const strip = $('term-strip');
  if (s === 'hidden') {
    termWrap.style.display = 'none';
    if (strip) strip.style.display = 'flex';
  } else {
    termWrap.style.display = '';
    if (strip) strip.style.display = 'none';
    termWrap.className = s === 'minimized' ? 'minimized' : '';
    if (s === 'visible') setTimeout(() => fitAddon.fit(), 60);
  }
  $('btn-term-minimize').textContent = s === 'minimized' ? 'Expand ▴' : 'Minimize ▾';
}
$('btn-term-minimize').addEventListener('click', () => setTermState(_termState === 'minimized' ? 'visible' : 'minimized'));
$('btn-term-hide').addEventListener('click',     () => setTermState('hidden'));
const _termStrip = $('term-strip');
if (_termStrip) _termStrip.addEventListener('click', () => setTermState('visible'));
$('btn-term-clear').addEventListener('click',    () => term.clear());

// Drag to resize — works from any part of the header except buttons
let _dy = 0, _dh = 0;
$('term-header').addEventListener('mousedown', e => {
  if (e.target.closest('button')) return;
  _dy = e.clientY; _dh = termWrap.offsetHeight;
  const mv = e2 => {
    if (_termState !== 'visible') return;
    const h = Math.max(80, _dh - (e2.clientY - _dy));
    termWrap.style.setProperty('--term-h', h + 'px');
    fitAddon.fit();
  };
  const up = () => { removeEventListener('mousemove', mv); removeEventListener('mouseup', up); };
  addEventListener('mousemove', mv);
  addEventListener('mouseup', up);
});

function tw(text, color) {
  const c = { cyan: '\x1b[36m', green: '\x1b[32m', yellow: '\x1b[33m', red: '\x1b[31m', reset: '\x1b[0m' };
  term.write((c[color] || '') + text + (color ? c.reset : ''));
}

let _lastCmd = '';
$('btn-term-copy').addEventListener('click', () => {
  if (_lastCmd) navigator.clipboard?.writeText(_lastCmd).catch(() => {});
});

// ─── WebSocket ───────────────────────────────────────────────────────────────

let _ws;
let _exitResolvers = [];

function connectWs() {
  _ws = new WebSocket(`ws://${location.host}/ws/terminal`);
  _ws.onmessage = e => {
    const msg = JSON.parse(e.data);
    if (msg.ping) return;
    if (msg.stream) {
      term.write(msg.data);
    } else if ('exit' in msg) {
      const ok = msg.exit === 0;
      tw(`\n— process exited (code ${msg.exit}) —\n`, ok ? 'green' : 'red');
      setStatus('idle');
      _stopElapsed();
      $('term-proc').textContent = '';
      $('run-status').textContent = '';
      if (msg.label === 'sudo-auth') {
        const ss = $('sudo-status');
        if (ok) {
          ss.textContent = '✓ Credentials cached — you can now launch benchmarks';
          ss.style.color = 'var(--green)';
        } else {
          ss.textContent = '✗ Authentication failed — try again';
          ss.style.color = 'var(--red)';
        }
        $('btn-sudo-auth').disabled = false;
      } else {
        const activeTab = document.querySelector('.tab-btn.active')?.dataset?.tab;
        if (activeTab === 'results') loadSessions();
      }
      enableRunControls();
      _exitResolvers.splice(0).forEach(fn => fn(msg.exit));
    }
  };
  _ws.onclose = () => setTimeout(connectWs, 1500);
}
connectWs();

function waitForExit() {
  return new Promise(r => _exitResolvers.push(r));
}

// ─── Status ──────────────────────────────────────────────────────────────────

function setStatus(state) {
  const colors = { idle: 'var(--green)', busy: 'var(--yellow)', error: 'var(--red)' };
  $('status-dot').style.background = colors[state] || colors.idle;
  $('status-label').textContent = state;
}

let _elapsedTimer = null;

function _startElapsed(startedAt) {
  if (_elapsedTimer) clearInterval(_elapsedTimer);
  function tick() {
    const secs = Math.floor(Date.now() / 1000 - startedAt);
    const h = Math.floor(secs / 3600);
    const m = Math.floor((secs % 3600) / 60);
    const s = secs % 60;
    const t = (h ? h + 'h ' : '') + (h || m ? String(m).padStart(2, '0') + 'm ' : '') + String(s).padStart(2, '0') + 's';
    $('run-status').textContent = 'Running… ' + t;
  }
  tick();
  _elapsedTimer = setInterval(tick, 1000);
}

function _stopElapsed() {
  if (_elapsedTimer) { clearInterval(_elapsedTimer); _elapsedTimer = null; }
}

async function pollStatus() {
  try {
    const s = await fetch('/api/status').then(r => r.json());
    setStatus(s.running ? 'busy' : 'idle');
    if (s.running) {
      $('term-proc').textContent = s.label || '';
      $('btn-stop').disabled = false;
      $('btn-launch').disabled = true;
      if (s.started && !_elapsedTimer) _startElapsed(s.started);
    } else {
      _stopElapsed();
    }
  } catch {}
  setTimeout(pollStatus, 3000);
}
pollStatus();

// ─── Make launcher ───────────────────────────────────────────────────────────

async function launchMake(target, server, envVars) {
  const body = { target, env: envVars || {} };
  if (server) body.server = server;
  const r = await fetch('/api/run', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body),
  });
  if (!r.ok) throw new Error((await r.json()).detail);
}

// ═══════════════════════════════════════════════════════════════════════════
// SETUP TAB
// ═══════════════════════════════════════════════════════════════════════════

async function loadSysinfo() {
  try {
    const si = await fetch('/api/sysinfo').then(r => r.json());
    $('si-status').textContent  = 'ready';
    $('si-cpu').textContent     = `${si.cpu_physical}C/${si.cpu_logical}T`;
    $('si-mem').textContent     = (si.memory_gb || 0) + ' GB';
    $('si-os').textContent      = si.os || '—';
    $('si-gov').textContent     = si.governor || '—';
    $('si-temp').textContent    = si.temp_current != null ? si.temp_current + '°C' : '—';
    $('si-scap').textContent    = si.scaphandre || '—';
    $('si-docker').textContent  = si.docker_ok ? 'ok' : 'not found';
    $('si-rapl').textContent    = si.rapl ? 'ok' : '—';
    $('si-filter').textContent  = si.filter || 'none';
    $('si-runs').textContent    = si.runs || '—';
    $('si-isolation').textContent = si.isolation || '—';
  } catch {}
}

async function loadConfig() {
  try {
    const cfg = await fetch('/api/config').then(r => r.json());
    const g  = (sec, key) => cfg[sec]?.[key] ?? '';
    const sv = (id, v)  => { const el = $(id); if (el) el.value = v; };
    const sc = (id, v)  => { const el = $(id); if (el) el.checked = v === 'true' || v === true; };
    const sr = (name, v) => {
      const el = document.querySelector(`input[name="${name}"][value="${v}"]`);
      if (el) el.checked = true;
    };
    sr('cfg-isolation', g('isolation', 'level') || 'basic');
    sv('cfg-runs',          g('measurement', 'runs') || 10);
    sv('cfg-confidence',    g('measurement', 'confidence') || 0.95);
    sv('cfg-baseline',      g('measurement', 'baseline_duration_s') || 10);
    sv('cfg-governor',      g('cpu', 'governor') || 'performance');
    sc('cfg-turbo',         g('cpu', 'disable_turbo'));
    sc('cfg-cstates',       g('cpu', 'disable_cstates'));
    sv('cfg-cpuset',        g('cpu', 'cpuset'));
    sc('cfg-thp',           g('memory', 'disable_thp'));
    sc('cfg-swap',          g('memory', 'check_swap'));
    sc('cfg-dropcaches',    g('memory', 'drop_caches'));
    sv('cfg-brightness',    g('display', 'brightness'));
    sc('cfg-screensaver',   g('display', 'disable_screensaver'));
    sv('cfg-services',      g('services', 'stop_before_run'));
    sv('cfg-cooltemp',      g('thermal', 'cooldown_temp_c'));
    sv('cfg-coolcpu',       g('thermal', 'cooldown_cpu_pct'));
    sv('cfg-cooltimeout',   g('thermal', 'cooldown_timeout_s'));
    sv('cfg-filter-model',  g('filter', 'model') || 'none');
    sv('cfg-contamination', g('filter', 'contamination') || 0.10);
    sv('cfg-iqr-factor',    g('filter', 'iqr_factor') || 1.5);
    sv('cfg-hampel-win',    g('filter', 'hampel_window') || 7);
    sv('cfg-hampel-thr',    g('filter', 'hampel_threshold') || 1.5);
    sv('cfg-dbscan-eps',    g('filter', 'dbscan_eps') || 150);
    sv('cfg-dbscan-mp',     g('filter', 'dbscan_minpts') || 10);
    sv('cfg-lof-n',         g('filter', 'lof_neighbors') || 20);
    sv('cfg-xrun-model',    g('cross_run_filter', 'model') || 'iqr');
    sv('cfg-xrun-factor',   g('cross_run_filter', 'factor') || 1.5);
    // Email notifications
    sv('cfg-email-from', g('notifications', 'email_from') || 'benchmark@localhost');
    sv('cfg-email-to',   g('notifications', 'email_to')   || '');
    const notifyOn = g('notifications', 'notify_on') || 'always';
    const noEl = $('cfg-notify-on');
    if (noEl) [...noEl.options].forEach(o => { if (o.value === notifyOn) o.selected = true; });
    // Reflect in sysinfo
    $('si-filter').textContent    = g('filter', 'model') || 'none';
    $('si-runs').textContent      = g('measurement', 'runs') || '—';
    $('si-isolation').textContent = g('isolation', 'level') || '—';
  } catch {}
}

$('btn-save-config').addEventListener('click', async () => {
  const rad = document.querySelector('input[name="cfg-isolation"]:checked');
  const body = {
    isolation:        { level: rad?.value || 'basic' },
    measurement:      { runs: $('cfg-runs').value, confidence: $('cfg-confidence').value, baseline_duration_s: $('cfg-baseline').value },
    cpu:              { governor: $('cfg-governor').value, disable_turbo: $('cfg-turbo').checked, disable_cstates: $('cfg-cstates').checked, cpuset: $('cfg-cpuset').value },
    memory:           { disable_thp: $('cfg-thp').checked, check_swap: $('cfg-swap').checked, drop_caches: $('cfg-dropcaches').checked },
    display:          { brightness: $('cfg-brightness').value, disable_screensaver: $('cfg-screensaver').checked },
    services:         { stop_before_run: $('cfg-services').value },
    thermal:          { cooldown_temp_c: $('cfg-cooltemp').value, cooldown_cpu_pct: $('cfg-coolcpu').value, cooldown_timeout_s: $('cfg-cooltimeout').value },
    filter:           { model: $('cfg-filter-model').value, contamination: $('cfg-contamination').value, iqr_factor: $('cfg-iqr-factor').value, hampel_window: $('cfg-hampel-win').value, hampel_threshold: $('cfg-hampel-thr').value, dbscan_eps: $('cfg-dbscan-eps').value, dbscan_minpts: $('cfg-dbscan-mp').value, lof_neighbors: $('cfg-lof-n').value },
    cross_run_filter: { model: $('cfg-xrun-model').value, factor: $('cfg-xrun-factor').value },
    notifications: {
      email_from: $('cfg-email-from').value.trim() || 'benchmark@localhost',
      email_to:   $('cfg-email-to').value.trim(),
      notify_on:  $('cfg-notify-on').value,
    },
  };
  try {
    await fetch('/api/config', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) });
    const s = $('cfg-save-status'); s.textContent = 'Saved.'; s.style.color = 'var(--green)';
    setTimeout(() => s.textContent = '', 2500);
    loadConfig();
  } catch {
    const s = $('cfg-save-status'); s.textContent = 'Save failed.'; s.style.color = 'var(--red)';
  }
});

$('btn-reload-sysinfo').addEventListener('click', () => { loadSysinfo(); loadConfig(); });

$('btn-profiler').addEventListener('click', async () => {
  setTermState('visible');
  setStatus('busy');
  tw('\n[GUI] Running profiler…\n', 'cyan');
  try {
    const r = await fetch('/api/profile', { method: 'POST' });
    if (!r.ok) tw('[ERROR] ' + (await r.json()).detail + '\n', 'red');
  } catch (e) { tw('[ERROR] ' + e + '\n', 'red'); }
});

// ═══════════════════════════════════════════════════════════════════════════
// RUN TAB
// ═══════════════════════════════════════════════════════════════════════════

// Full HTTP request levels as used by run_benchmarks.sh
const HTTP_LEVELS = {
  full:       [100, 1000, 5000, 8000, 10000, 15000, 20000, 30000, 40000, 50000, 60000, 70000, 80000],
  quick:      [1000, 5000, 10000],
  superquick: [1000],
};

// Make targets — names and descriptions mirror the Makefile exactly
const TARGETS = [
  { value: 'run-super-quick',        name: 'Super Quick',      cmd: 'make run-super-quick',
    desc: 'All containers · 1 HTTP level + 1 WS burst/stream', levels: 'superquick', needsServer: false },
  { value: 'run-quick',              name: 'Quick',            cmd: 'make run-quick',
    desc: 'All containers · 3 HTTP levels + quick WS',         levels: 'quick',      needsServer: false },
  { value: 'run',                    name: 'Full',             cmd: 'make run',
    desc: 'All containers · 13 HTTP levels + full WS',         levels: 'full',       needsServer: false },
  { value: 'run-static',             name: 'Static only',      cmd: 'make run-static',
    desc: 'Static containers · 13 HTTP levels',                levels: 'full',       needsServer: false },
  { value: 'run-dynamic',            name: 'Dynamic only',     cmd: 'make run-dynamic',
    desc: 'Dynamic containers · 13 HTTP levels',               levels: 'full',       needsServer: false },
  { value: 'run-websocket',          name: 'WebSocket only',   cmd: 'make run-websocket',
    desc: 'WS containers · burst · stream · concurrency · payload',
    levels: 'ws',         needsServer: false },
  { value: 'run-single',             name: 'Single (full)',    cmd: 'make run-single SERVER=…',
    desc: 'One container · full levels for its type',          levels: 'full',       needsServer: true  },
  { value: 'run-single-super-quick', name: 'Single (quick)',   cmd: 'make run-single-super-quick SERVER=…',
    desc: 'One container · 1 request level (super-quick)',     levels: 'superquick', needsServer: true  },
];

let _selectedTarget = TARGETS[0];
let _containers     = [];

function buildTargetGrid() {
  const grid = $('target-grid');
  grid.innerHTML = '';
  TARGETS.forEach(t => {
    const btn = document.createElement('button');
    btn.className = 'target-card' + (t.value === _selectedTarget.value ? ' selected' : '');
    btn.innerHTML = `<div class="tc-name">${t.name}</div><div class="tc-cmd">${t.cmd}</div><div class="tc-desc">${t.desc}</div>`;
    btn.addEventListener('click', () => { _selectedTarget = t; buildTargetGrid(); updateRunPreview(); });
    grid.appendChild(btn);
  });
}

function updateRunPreview() {
  const t = _selectedTarget;
  $('single-wrap').style.display = t.needsServer ? '' : 'none';

  // Payload chips
  const chips = $('run-chips');
  const wrap  = $('run-payload-wrap');
  if (t.levels === 'ws') {
    wrap.style.display = '';
    chips.innerHTML = `
      <span class="p-chip">5 clients</span>
      <span class="p-chip">50 clients</span>
      <span class="p-chip">100 clients</span>
      <span class="p-chip" style="margin-left:8px;font-style:italic">burst · stream · concurrency · payload</span>`;
  } else if (t.levels) {
    wrap.style.display = '';
    chips.innerHTML = HTTP_LEVELS[t.levels].map(n => `<span class="p-chip">${n.toLocaleString()}</span>`).join('');
  } else {
    wrap.style.display = 'none';
  }

  // Summary line
  const st = _containers.filter(c => c.type === 'static').length;
  const dy = _containers.filter(c => c.type === 'dynamic').length;
  const ws = _containers.filter(c => c.type === 'websocket').length;
  let summary = '';
  if (_containers.length) {
    if      (t.value.includes('static'))    summary = `${st} static container${st !== 1 ? 's' : ''}`;
    else if (t.value.includes('dynamic'))   summary = `${dy} dynamic container${dy !== 1 ? 's' : ''}`;
    else if (t.value.includes('websocket')) summary = `${ws} WebSocket container${ws !== 1 ? 's' : ''}`;
    else if (t.needsServer)                 summary = 'Selected container only';
    else                                    summary = `${st} static + ${dy} dynamic + ${ws} WebSocket`;
    if (t.levels && t.levels !== 'ws' && !t.needsServer) {
      summary += ` × ${HTTP_LEVELS[t.levels].length} request level${HTTP_LEVELS[t.levels].length !== 1 ? 's' : ''}`;
    }
  }
  $('run-summary').textContent = summary;

  // Command preview
  let cmd = 'make ' + t.value;
  if (t.needsServer) {
    const server = $('run-single-cont')?.value || '';
    if (server) cmd += ' SERVER=' + server;
  }
  const envStr = buildEnvString();
  if (envStr) cmd = envStr + ' ' + cmd;
  _lastCmd = 'make ' + t.value + (t.needsServer && $('run-single-cont')?.value ? ' SERVER=' + $('run-single-cont').value : '');
  $('run-cmd-preview').textContent = '$ ' + cmd;
}

function buildEnvVars() {
  const env = {};
  const bd = $('env-bench-dir')?.value.trim();
  const w  = $('env-http-workers')?.value.trim();
  const p  = $('env-host-port')?.value.trim();
  const q  = $('env-quiet')?.value;
  const sw = $('env-startup-wait')?.value.trim();
  const hr = $('env-health-retries')?.value.trim();
  const hb = $('env-heartbeat')?.value.trim();
  if (bd) env['BENCH_DIR']             = bd;
  if (w)  env['HTTP_MAX_WORKERS']      = w;
  if (p)  env['HOST_PORT']             = p;
  if (q)  env['BENCH_MEASURE_QUIET']   = q;
  if (sw) env['MEASURE_STARTUP_WAIT']  = sw;
  if (hr) env['MEASURE_HEALTH_RETRIES']= hr;
  if (hb) env['MEASURE_HEARTBEAT_SEC'] = hb;
  return env;
}

function buildEnvString() {
  return Object.entries(buildEnvVars()).map(([k,v]) => `${k}=${v}`).join(' ');
}

async function loadRunContainers() {
  const el = $('run-containers');
  try {
    const benchDir = $('env-bench-dir')?.value.trim();
    const url = '/api/containers' + (benchDir ? '?bench_dir=' + encodeURIComponent(benchDir) : '');
    _containers = await fetch(url).then(r => r.json());
    if (!_containers.length) {
      el.innerHTML = '<span class="muted">No containers found. Add Dockerfiles under <code>benchmarks/{type}/{name}/</code>.</span>';
      return;
    }
    const st = _containers.filter(c => c.type === 'static').length;
    const dy = _containers.filter(c => c.type === 'dynamic').length;
    const ws = _containers.filter(c => c.type === 'websocket').length;
    el.innerHTML = `
      <div style="display:flex;gap:8px;flex-wrap:wrap;margin-bottom:8px">
        <span class="badge badge-s">${st} static</span>
        <span class="badge badge-d">${dy} dynamic</span>
        <span class="badge badge-w">${ws} websocket</span>
        <span class="muted" style="margin-left:4px">${_containers.length} total</span>
      </div>
      <div class="cont-list" style="max-height:160px;overflow-y:auto">
        ${_containers.map(c => `
          <div class="cont-item">
            <span class="badge badge-${c.type[0]}">${c.type[0].toUpperCase()}</span>
            <span class="ci-name">${c.name}</span>
          </div>`).join('')}
      </div>`;

    const sel = $('run-single-cont');
    sel.innerHTML = _containers.map(c => `<option value="${c.name}">${c.name} (${c.type})</option>`).join('');
    sel.addEventListener('change', updateRunPreview);
    const qSel = $('q-server');
    if (qSel) {
      const qCur = qSel.value;
      qSel.innerHTML = '<option value="">— any / not applicable —</option>' +
        _containers.map(c => `<option value="${c.name}">${c.name} (${c.type})</option>`).join('');
      if (qCur) qSel.value = qCur;
    }

    buildTargetGrid();
    updateRunPreview();
  } catch {
    el.textContent = 'Failed to load containers.';
  }
}

function enableRunControls() {
  $('btn-launch').disabled = false;
  $('btn-stop').disabled   = true;
}

// ── Sudo auth ────────────────────────────────────────────────────────────────

$('btn-sudo-auth').addEventListener('click', async () => {
  if (mgr_running()) return;
  setTermState('visible');
  setTimeout(() => term.focus(), 80);
  tw('\n[GUI] sudo -v  — type your password below, then press Enter\n', 'yellow');
  $('sudo-status').textContent = 'Waiting for password…';
  $('sudo-status').style.color = 'var(--yellow)';
  $('btn-sudo-auth').disabled = true;
  try {
    const r = await fetch('/api/sudo-auth', { method: 'POST' });
    if (!r.ok) {
      tw('[ERROR] ' + (await r.json()).detail + '\n', 'red');
      $('sudo-status').textContent = 'Failed.';
      $('sudo-status').style.color = 'var(--red)';
      $('btn-sudo-auth').disabled = false;
    }
    // Result shown via WebSocket exit message
  } catch (e) {
    tw('[ERROR] ' + e + '\n', 'red');
    $('btn-sudo-auth').disabled = false;
  }
});

function mgr_running() {
  return $('btn-launch').disabled && !$('btn-stop').disabled;
}

// Re-render command preview when env fields change
['env-bench-dir','env-http-workers','env-host-port','env-quiet','env-startup-wait','env-health-retries','env-heartbeat']
  .forEach(id => { const el = $(id); if (el) el.addEventListener('input', updateRunPreview); });
// Reload container list when BENCH_DIR changes (debounced)
let _benchDirTimer = null;
const _benchDirEl = $('env-bench-dir');
if (_benchDirEl) _benchDirEl.addEventListener('input', () => {
  clearTimeout(_benchDirTimer);
  _benchDirTimer = setTimeout(loadRunContainers, 600);
});

$('btn-launch').addEventListener('click', async () => {
  const t = _selectedTarget;
  const server = t.needsServer ? ($('run-single-cont')?.value || '') : '';
  const envVars = buildEnvVars();
  $('btn-launch').disabled = true;
  $('btn-stop').disabled   = false;
  setStatus('busy');
  setTermState('visible');
  setTimeout(() => term.focus(), 80);
  const displayCmd = 'make ' + t.value + (server ? ' SERVER=' + server : '');
  _lastCmd = displayCmd;
  $('run-cmd-preview').textContent = '$ ' + (buildEnvString() ? buildEnvString() + ' ' : '') + displayCmd;
  tw('\n[GUI] ' + displayCmd + '\n', 'cyan');
  tw('[INFO] Benchmark runs as a detached job — the GUI stays responsive.\n', 'yellow');
  tw('[INFO] Output is streamed from the log file below.\n', 'yellow');
  $('run-status').textContent = 'Running…';
  $('term-proc').textContent  = t.value;
  try {
    const r = await fetch('/api/run', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ target: t.value, server: server || undefined, env: envVars }),
    });
    if (!r.ok) throw new Error((await r.json()).detail);
    const data = await r.json();
    if (data.log) tw('[LOG] ' + data.log + '\n', 'cyan');
  } catch (e) {
    tw('[ERROR] ' + e.message + '\n', 'red');
    enableRunControls();
    setStatus('idle');
  }
});

// Utility buttons (init, setup, build, validate, check-health, test, etc.)
document.querySelectorAll('.util-btn').forEach(btn => {
  btn.addEventListener('click', async () => {
    const target = btn.dataset.target;
    if (!target) return;
    const confirmed = ['clean-results','clean-build','clean-env'].includes(target)
      ? confirm('Run: make ' + target + '?') : true;
    if (!confirmed) return;
    const env = {};
    if (target === 'clean-port') {
      const port = $('util-port')?.value?.trim();
      if (!port) { tw('\n[ERROR] Enter a port number before running clean-port\n', 'red'); return; }
      env.PORT = port;
    }
    const cmdLabel = 'make ' + target + (env.PORT ? ' PORT=' + env.PORT : '');
    setTermState('visible');
    setStatus('busy');
    tw('\n[GUI] ' + cmdLabel + '\n', 'cyan');
    $('term-proc').textContent = target;
    try {
      const r = await fetch('/api/run', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ target, env }),
      });
      if (!r.ok) tw('[ERROR] ' + (await r.json()).detail + '\n', 'red');
    } catch (e) { tw('[ERROR] ' + e + '\n', 'red'); }
  });
});

$('btn-stop').addEventListener('click', async () => {
  await fetch('/api/stop', { method: 'POST' });
  $('btn-stop').disabled = true;
});

// ── Queue ────────────────────────────────────────────────────────────────────

let _queue = [], _qRunning = false;

$('btn-q-add').addEventListener('click', () => {
  _queue.push({ target: $('q-target').value, server: $('q-server').value.trim(), state: null });
  renderQueue();
});
$('btn-q-clear').addEventListener('click', () => { _queue = []; renderQueue(); });

function renderQueue() {
  const list = $('queue-list');
  $('btn-q-run').disabled = _qRunning || !_queue.length;
  if (!_queue.length) {
    list.innerHTML = '<span class="muted">Queue is empty.</span>';
    return;
  }
  list.innerHTML = '';
  _queue.forEach((e, i) => {
    const d = document.createElement('div');
    d.className = 'q-item' + (e.state ? ' ' + e.state : '');
    const cmd = 'make ' + e.target + (e.server ? ' SERVER=' + e.server : '');
    d.innerHTML = `<span class="qi">${i + 1}</span><span class="qc">${cmd}</span>
      <span class="qs">${e.state === 'running' ? '⟳' : e.state === 'done' ? '✓' : e.state === 'failed' ? '✗' : ''}</span>
      <button class="qr" data-i="${i}">×</button>`;
    d.querySelector('.qr').addEventListener('click', ev => {
      if (_qRunning) return;
      _queue.splice(+ev.target.dataset.i, 1);
      renderQueue();
    });
    list.appendChild(d);
  });
}

$('btn-q-run').addEventListener('click', async () => {
  if (_qRunning || !_queue.length) return;
  _qRunning = true;
  $('btn-q-run').disabled = true;
  setTermState('visible');
  for (let i = 0; i < _queue.length; i++) {
    const e = _queue[i];
    e.state = 'running'; renderQueue();
    $('q-status').textContent = `${i + 1}/${_queue.length}: ${e.target}`;
    setStatus('busy');
    const cmd = 'make ' + e.target + (e.server ? ' SERVER=' + e.server : '');
    tw(`\n[QUEUE ${i + 1}/${_queue.length}] ${cmd}\n`, 'cyan');
    _lastCmd = cmd;
    try {
      await launchMake(e.target, e.server || '');
      const code = await waitForExit();
      e.state = code === 0 ? 'done' : 'failed';
    } catch (err) {
      tw('[ERROR] ' + err.message + '\n', 'red');
      e.state = 'failed';
    }
    renderQueue();
  }
  _qRunning = false;
  renderQueue();
  const ok = _queue.filter(e => e.state === 'done').length;
  const fail = _queue.filter(e => e.state === 'failed').length;
  $('q-status').textContent = `Done. ${ok} succeeded, ${fail} failed.`;
  tw(`\n[QUEUE] Complete. ${ok} succeeded, ${fail} failed.\n`, ok === _queue.length ? 'green' : 'yellow');
  setStatus('idle');
});

// ═══════════════════════════════════════════════════════════════════════════
// RESULTS TAB
// ═══════════════════════════════════════════════════════════════════════════

let _sessions      = [];
let _activeSession = null;
let _activeFile    = null;
let _currentRows   = [];

async function loadSessions() {
  const list = $('session-list');
  list.innerHTML = '<span class="muted">Loading…</span>';
  try {
    _sessions = await fetch('/api/results/sessions').then(r => r.json());
    if (!_sessions.length) { list.innerHTML = '<span class="muted">No sessions yet. Run a benchmark first.</span>'; return; }
    list.innerHTML = '';
    _sessions.forEach(s => {
      const d = document.createElement('div');
      d.className = 'sess-item';
      d.innerHTML = `<div class="sn">${s.name}</div><div class="st">${(s.types || []).join(' · ')}</div>`;
      d.addEventListener('click', () => selectSession(s, d));
      list.appendChild(d);
    });
    selectSession(_sessions[0], list.firstChild);
  } catch { list.innerHTML = '<span class="muted">Failed to load sessions.</span>'; }
}

async function selectSession(session, el) {
  document.querySelectorAll('#session-list .sess-item').forEach(e => e.classList.remove('active'));
  el?.classList.add('active');
  _activeSession = session;
  _activeFile    = null;

  const fl = $('file-list');
  fl.innerHTML = '<span class="muted">Loading…</span>';
  try {
    const files = await fetch('/api/results?path=' + encodeURIComponent(session.path)).then(r => r.json());
    if (!files.length) { fl.innerHTML = '<span class="muted">No CSV files in this session.</span>'; return; }
    fl.innerHTML = '';
    // Show path relative to session dir (e.g. "static/st-cowboy-27-self.csv")
    const sessionPrefix = session.path.replace(/\\/g, '/').replace(/\/?$/, '/');
    files.forEach(f => {
      const relToSession = (f.path.replace(/\\/g, '/') + '').split(sessionPrefix)[1] || f.rel || f.name;
      const d = document.createElement('div');
      d.className = 'file-item';
      d.innerHTML = `<span class="fi-name" title="${f.rel}">${relToSession}</span><span class="fi-sz">${fmtBytes(f.size)}</span>`;
      d.addEventListener('click', () => selectFile(f, d));
      fl.appendChild(d);
    });
    selectFile(files[0], fl.firstChild);
  } catch { fl.innerHTML = '<span class="muted">Failed to load files.</span>'; }
}

async function selectFile(file, el) {
  document.querySelectorAll('#file-list .file-item').forEach(e => e.classList.remove('active'));
  el?.classList.add('active');
  _activeFile = file;
  $('results-status').textContent = 'Loading…';
  try {
    _currentRows = await fetch('/api/results/file?path=' + encodeURIComponent(file.path)).then(r => r.json());
    drawResultsChart();
    $('results-status').textContent = `${_currentRows.length} row(s) · ${file.rel || file.name}`;
  } catch { $('results-status').textContent = 'Failed to load file.'; }
}

function drawResultsChart() {
  if (!_currentRows.length) return;
  const metric    = $('results-metric').value;
  const showCi    = $('results-ci').checked;
  let   chartType = $('results-chart-type').value;

  // Auto: if multiple distinct request levels → line; otherwise bar
  if (chartType === 'auto') {
    const reqLevels = new Set(_currentRows.map(r => r['Total Requests']));
    chartType = reqLevels.size > 1 ? 'line' : 'bar';
  }
  renderResultChart(_currentRows, metric, chartType, showCi);
}

$('results-metric').addEventListener('change',     drawResultsChart);
$('results-chart-type').addEventListener('change', drawResultsChart);
$('results-ci').addEventListener('change',         drawResultsChart);
$('btn-refresh-results').addEventListener('click', loadSessions);

// ═══════════════════════════════════════════════════════════════════════════
// EXPORT TAB
// ═══════════════════════════════════════════════════════════════════════════

const EXPORT_METRICS = [
  'Total Energy (J)', 'Avg Power (W)', 'Execution Time (s)', 'Requests/s',
  'Successful Requests', 'Failed Requests', 'Avg CPU (%)', 'Peak CPU (%)',
  'Avg Mem (MB)', 'Peak Mem (MB)',
];
const SIZE_PRESETS = {
  column: { w: 320, h: 240 }, half: { w: 454, h: 340 },
  full:   { w: 658, h: 440 }, a4:   { w: 1122, h: 794 }, screen: null,
};

function buildExportMetrics() {
  const grid = $('exp-metric-grid');
  grid.innerHTML = '';
  EXPORT_METRICS.forEach(m => {
    const lbl = document.createElement('label');
    lbl.className = 'metric-chk';
    const chk = document.createElement('input');
    chk.type = 'checkbox'; chk.value = m; chk.checked = true;
    lbl.appendChild(chk);
    lbl.appendChild(document.createTextNode(m));
    grid.appendChild(lbl);
  });
}
$('btn-exp-selall').addEventListener('click',  () => document.querySelectorAll('#exp-metric-grid input').forEach(c => c.checked = true));
$('btn-exp-selnone').addEventListener('click', () => document.querySelectorAll('#exp-metric-grid input').forEach(c => c.checked = false));

async function loadExportSessions() {
  const sel = $('exp-session');
  const cur = sel.value;
  const sessions = await fetch('/api/results/sessions').then(r => r.json()).catch(() => []);
  sel.innerHTML = '<option value="">— select session —</option>';
  sessions.forEach(s => {
    const o = document.createElement('option');
    o.value = s.path; o.textContent = s.name;
    sel.appendChild(o);
  });
  if (cur) sel.value = cur;
}

$('exp-session').addEventListener('change', async () => {
  const path = $('exp-session').value;
  const fileSel = $('exp-file');
  fileSel.innerHTML = '<option value="">— select file —</option>';
  if (!path) return;
  const files = await fetch('/api/results?path=' + encodeURIComponent(path)).then(r => r.json()).catch(() => []);
  files.forEach(f => {
    const o = document.createElement('option');
    o.value = f.path; o.textContent = f.rel || f.name;
    fileSel.appendChild(o);
  });
});

$('btn-exp-chart').addEventListener('click', async () => {
  const filePath = $('exp-file').value;
  if (!filePath) { $('exp-status').textContent = 'Select a file first.'; return; }
  const rows = await fetch('/api/results/file?path=' + encodeURIComponent(filePath)).then(r => r.json()).catch(() => []);
  if (!rows.length) { $('exp-status').textContent = 'No data.'; return; }
  const metrics = [...document.querySelectorAll('#exp-metric-grid input:checked')].map(c => c.value);
  const format  = $('exp-format').value;
  const sizeKey = $('exp-size').value;
  const style   = $('exp-style').value;
  const dpi     = parseInt($('exp-dpi').value) || 150;
  $('exp-status').textContent = `Exporting ${metrics.length} chart(s)…`;
  for (const m of metrics) {
    await exportChart(rows, m, format, sizeKey, style, dpi);
    await new Promise(r => setTimeout(r, 120));
  }
  $('exp-status').textContent = `Exported ${metrics.length} chart(s).`;
});

$('btn-exp-csv').addEventListener('click', async () => {
  const filePath = $('exp-file').value;
  if (!filePath) { $('exp-status').textContent = 'Select a file first.'; return; }
  const rows = await fetch('/api/results/file?path=' + encodeURIComponent(filePath)).then(r => r.json()).catch(() => []);
  if (!rows.length) return;
  const headers = Object.keys(rows[0]);
  const csv = [headers.join(','), ...rows.map(r => headers.map(h => JSON.stringify(r[h] ?? '')).join(','))].join('\n');
  const a = document.createElement('a');
  a.href = 'data:text/csv;charset=utf-8,' + encodeURIComponent(csv);
  a.download = filePath.split('/').pop();
  a.click();
  $('exp-status').textContent = 'CSV downloaded.';
});

async function exportChart(rows, metric, format, sizeKey, style, dpi) {
  const preset = SIZE_PRESETS[sizeKey];
  const w = preset ? Math.round(preset.w * dpi / 96) : 800;
  const h = preset ? Math.round(preset.h * dpi / 96) : 500;
  window._exportStyle = style === 'paper' ? 'paper' : 'color';
  const canvas = document.createElement('canvas');
  canvas.width = w; canvas.height = h;
  const reqLevels = new Set(rows.map(r => r['Total Requests']));
  const chartType = reqLevels.size > 1 ? 'line' : 'bar';
  const chart = renderResultChart(rows, metric, chartType, false, canvas);
  await new Promise(r => setTimeout(r, 80));
  const fname = metric.replace(/[^a-z0-9]+/gi, '_').replace(/_+$/, '') + '.' + (format === 'pdf' ? 'png' : format);
  const a = document.createElement('a');
  a.download = fname;
  a.href = canvas.toDataURL('image/png');
  a.click();
  if (chart) chart.destroy();
  window._exportStyle = null;
}

// ═══════════════════════════════════════════════════════════════════════════
// MODEL SELECTOR TAB
// ═══════════════════════════════════════════════════════════════════════════
//
// Correct flow:
//   - One benchmark session = one `make run` invocation
//   - CV scoring needs ≥ 3 separate sessions of the same containers
//   - Container names come from session CSVs (reliable), not JSON consumers (unreliable on cgroups v2)
//   - JSON files for a session are found by timestamp: session dir name → start time;
//     last CSV mtime → end time; JSON files whose filename timestamp falls in that window
//

let _msSessions   = [];   // available sessions (checkboxes)
let _msContainer  = null; // selected container name
let _msJsonFiles  = [];   // json files from selected sessions
let _msCsvContainers = []; // containers found in selected sessions' CSVs

async function loadMsSessions() {
  const list = $('ms-session-list');
  list.innerHTML = '<span class="muted">Loading…</span>';
  try {
    _msSessions = await fetch('/api/results/sessions').then(r => r.json());
    if (!_msSessions.length) {
      list.innerHTML = '<span class="muted">No sessions yet. Run a benchmark first.</span>';
      return;
    }
    list.innerHTML = '';
    _msSessions.forEach(s => {
      const lbl = document.createElement('label');
      lbl.style.cssText = 'display:flex;align-items:flex-start;gap:8px;padding:6px 4px;cursor:pointer;font-size:12px;border-radius:4px';
      lbl.onmouseenter = () => lbl.style.background = 'var(--surf2)';
      lbl.onmouseleave = () => lbl.style.background = '';
      const chk = document.createElement('input');
      chk.type = 'checkbox'; chk.value = JSON.stringify({ path: s.path, name: s.name });
      chk.style.marginTop = '2px';
      chk.addEventListener('change', updateMsSessionCount);
      const info = document.createElement('span');
      info.innerHTML = `<span style="font-family:var(--mono);color:var(--accent)">${s.name}</span>
        <span style="color:var(--muted);margin-left:6px">${(s.types || []).join(' · ')}</span>`;
      lbl.appendChild(chk); lbl.appendChild(info);
      list.appendChild(lbl);
    });
  } catch { list.innerHTML = '<span class="muted">Failed to load sessions.</span>'; }
}

function updateMsSessionCount() {
  const selected = [...document.querySelectorAll('#ms-session-list input:checked')].length;
  const warn = $('ms-cv-warn');
  const found = $('ms-sessions-found');
  if (found) found.textContent = selected;
  if (warn)  warn.style.display = selected > 0 && selected < 3 ? '' : 'none';
}

$('btn-ms-scan').addEventListener('click', async () => {
  const checked = [...document.querySelectorAll('#ms-session-list input:checked')];
  if (!checked.length) {
    $('ms-scan-status').textContent = 'Select at least one session first.';
    return;
  }
  const sessions = checked.map(c => JSON.parse(c.value));
  const folder   = $('ms-json-folder').value.trim() || 'output';

  $('ms-cont-card').style.display    = 'none';
  $('ms-config-card').style.display  = 'none';
  $('ms-results-card').style.display = 'none';
  $('ms-scan-status').textContent    = 'Scanning…';
  _msJsonFiles     = [];
  _msCsvContainers = [];
  _msContainer     = null;

  try {
    // Collect JSON files and container names from all selected sessions
    const allJsonFiles = [];
    const containerSet = new Set();

    for (const s of sessions) {
      // JSON files for this session (by timestamp window)
      const files = await fetch(
        '/api/json-for-session?session_path=' + encodeURIComponent(s.path) + '&folder=' + encodeURIComponent(folder)
      ).then(r => r.json()).catch(() => []);
      allJsonFiles.push(...files);

      // Containers from this session's CSVs (reliable)
      const conts = await fetch(
        '/api/session-containers?session_path=' + encodeURIComponent(s.path)
      ).then(r => r.json()).catch(() => []);
      conts.forEach(c => containerSet.add(c));
    }

    _msJsonFiles     = allJsonFiles;
    _msCsvContainers = [...containerSet].sort();

    $('ms-scan-status').textContent = `${sessions.length} session(s) · ${allJsonFiles.length} JSON file(s) · ${_msCsvContainers.length} container(s)`;

    if (!_msCsvContainers.length) {
      $('ms-scan-status').textContent += ' — no containers found in session CSVs.';
      return;
    }

    renderMsContainers(_msCsvContainers);
    $('ms-cont-card').style.display   = '';
    $('ms-config-card').style.display = '';
    updateMsCmd();

  } catch (e) {
    $('ms-scan-status').textContent = 'Scan failed: ' + e;
  }
});

function renderMsContainers(names) {
  const list = $('ms-cont-list');
  list.innerHTML = '';
  names.forEach((name, i) => {
    const d = document.createElement('div');
    d.className = 'cont-item';
    d.style.cursor = 'pointer';
    // Infer type from name prefix (st- / dy- / ws-)
    const typeClass = name.startsWith('st-') ? 's' : name.startsWith('dy-') ? 'd' : name.startsWith('ws-') ? 'w' : 's';
    d.innerHTML = `<span class="badge badge-${typeClass}">${typeClass.toUpperCase()}</span><span class="ci-name">${name}</span>`;
    if (i === 0) { d.style.borderColor = 'var(--accent)'; _msContainer = name; }
    d.addEventListener('click', () => {
      list.querySelectorAll('.cont-item').forEach(x => x.style.borderColor = '');
      d.style.borderColor = 'var(--accent)';
      _msContainer = name;
      updateMsCmd();
    });
    list.appendChild(d);
  });
  $('ms-json-count').textContent = `${_msJsonFiles.length} JSON file(s) will be analysed.`;
}

function updateMsCmd() {
  if (!_msContainer) return;
  const metric = $('ms-metric').value;
  const apply  = $('ms-apply').checked;
  const folder = $('ms-json-folder').value.trim() || 'output';
  let cmd = `python3 tools/model_selector.py --input "${folder}/*.json" --container ${_msContainer} --metric ${metric}`;
  if (apply) cmd += ' --apply';
  $('ms-cmd-preview').textContent = '$ ' + cmd;
  _lastCmd = cmd;
}

$('ms-metric').addEventListener('change', updateMsCmd);
$('ms-apply').addEventListener('change',  updateMsCmd);

$('btn-ms-run').addEventListener('click', async () => {
  if (!_msContainer || !_msJsonFiles.length) return;
  const folder = $('ms-json-folder').value.trim() || 'output';
  const body = {
    container:  _msContainer,
    metric:     $('ms-metric').value,
    apply:      $('ms-apply').checked,
    json_files: _msJsonFiles,
  };

  $('ms-results-card').style.display = 'none';
  setTermState('visible');
  setStatus('busy');
  updateMsCmd();
  tw('\n[GUI] Running model_selector.py…\n', 'cyan');
  tw('[GUI] ' + $('ms-cmd-preview').textContent.replace(/^\$ /, '') + '\n', 'cyan');
  $('term-proc').textContent = 'model-selector';

  try {
    const r = await fetch('/api/model-selector', {
      method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body),
    });
    if (!r.ok) {
      tw('[ERROR] ' + (await r.json()).detail + '\n', 'red');
      setStatus('idle');
      return;
    }
    const code = await waitForExit();
    if (code === 0) {
      const results = await fetch('/api/model-selector-results').then(r => r.ok ? r.json() : null).catch(() => null);
      if (results?.rows?.length) {
        renderMsResults(results);
        $('ms-results-card').style.display = '';
        $('ms-results-card').scrollIntoView({ behavior: 'smooth', block: 'start' });
      }
    }
  } catch (e) {
    tw('[ERROR] ' + e + '\n', 'red');
    setStatus('idle');
  }
});

function renderMsResults(data) {
  const tbody    = $('ms-result-body');
  tbody.innerHTML = '';
  const maxScore = Math.max(...data.rows.map(r => r.score));
  data.rows.forEach(row => {
    const tr = document.createElement('tr');
    if (row.winner) tr.className = 'winner';
    const pct = maxScore > 0 ? Math.round(row.score / maxScore * 100) : 0;
    tr.innerHTML = `
      <td>${row.model}${row.winner ? ' ★' : ''}</td>
      <td>${row.score.toFixed(4)}</td>
      <td>${row.mean_w != null ? row.mean_w.toFixed(4) : '—'}</td>
      <td>${row.kept_pct != null ? row.kept_pct.toFixed(1) + '%' : '—'}</td>
      <td>${row.runs}</td>
      <td><div class="ms-bar-wrap"><div class="ms-bar" style="width:${pct}%"></div></div></td>`;
    tbody.appendChild(tr);
  });
  const label = data.metric === 'cv' ? 'CV (lower = more consistent)' : 'Error % (lower = better recovery)';
  $('ms-winner-note').textContent = `Recommended: ${data.winner}  —  ${label}`;
}

// ═══════════════════════════════════════════════════════════════════════════
// DIRECTORY BROWSER MODAL
// ═══════════════════════════════════════════════════════════════════════════

let _dirCallback = null;
let _dirCurrent  = '';

async function _dirBrowse(path) {
  try {
    const data = await fetch('/api/browse?path=' + encodeURIComponent(path || '.')).then(r => r.json());
    _dirCurrent = data.path;
    $('dir-modal-crumb').textContent = data.path;
    const list = $('dir-modal-list');
    list.innerHTML = '';
    if (data.parent) {
      const d = document.createElement('div');
      d.className = 'dir-entry';
      d.innerHTML = '<span class="de-icon">⬆</span><span class="de-name">..</span>';
      d.addEventListener('click', () => _dirBrowse(data.parent));
      list.appendChild(d);
    }
    data.entries.filter(e => e.type === 'dir').forEach(e => {
      const d = document.createElement('div');
      d.className = 'dir-entry';
      d.innerHTML = `<span class="de-icon">📁</span><span class="de-name">${e.name}</span>`;
      d.addEventListener('dblclick', () => _dirBrowse(e.path));
      d.addEventListener('click', () => {
        list.querySelectorAll('.dir-entry').forEach(x => x.style.background = '');
        d.style.background = 'var(--surf2)';
        _dirCurrent = e.path;
        $('dir-modal-crumb').textContent = e.path;
      });
      list.appendChild(d);
    });
    if (!data.entries.filter(e => e.type === 'dir').length) {
      list.innerHTML += '<div class="muted" style="padding:8px 10px">No subdirectories</div>';
    }
  } catch (err) {
    $('dir-modal-crumb').textContent = 'Error: ' + err;
  }
}

function openDirModal(initialPath, callback) {
  _dirCallback = callback;
  $('dir-modal').style.display = 'flex';
  _dirBrowse(initialPath || '.');
}

$('dir-modal-select').addEventListener('click', () => {
  $('dir-modal').style.display = 'none';
  if (_dirCallback) { _dirCallback(_dirCurrent); _dirCallback = null; }
});
$('dir-modal-cancel').addEventListener('click', () => { $('dir-modal').style.display = 'none'; _dirCallback = null; });
$('dir-modal-close').addEventListener('click',  () => { $('dir-modal').style.display = 'none'; _dirCallback = null; });
$('dir-modal').addEventListener('click', e => { if (e.target === $('dir-modal')) { $('dir-modal').style.display = 'none'; _dirCallback = null; } });

// BENCH_DIR browse button
$('btn-bench-dir-browse').addEventListener('click', () => {
  openDirModal($('env-bench-dir').value.trim() || '.', path => {
    $('env-bench-dir').value = path;
    loadRunContainers();
    updateRunPreview();
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// QUEUE — Add all containers
// ═══════════════════════════════════════════════════════════════════════════

$('btn-q-add-all').addEventListener('click', () => {
  const target = $('q-target').value;
  if (target.includes('single') && _containers.length) {
    _containers.forEach(c => _queue.push({ target, server: c.name, state: null }));
  } else {
    _queue.push({ target, server: '', state: null });
  }
  renderQueue();
});

// ═══════════════════════════════════════════════════════════════════════════
// RESULTS — custom folder path
// ═══════════════════════════════════════════════════════════════════════════

async function loadFilesFromFolder(folderPath) {
  const list = $('file-list');
  list.innerHTML = '<span class="muted">Loading…</span>';
  const sessionList = $('session-list');
  sessionList.innerHTML = '<span class="muted" style="padding:4px 8px;display:block">Custom folder</span>';
  try {
    const files = await fetch('/api/results?path=' + encodeURIComponent(folderPath)).then(r => r.json());
    if (!files.length) { list.innerHTML = '<span class="muted">No CSV files found.</span>'; return; }
    list.innerHTML = '';
    files.forEach(f => {
      const d = document.createElement('div');
      d.className = 'file-item';
      d.innerHTML = `<span class="fi-name" title="${f.rel || f.path}">${f.rel || f.name}</span><span class="fi-sz">${fmtBytes(f.size)}</span>`;
      d.addEventListener('click', () => selectFile(f, d));
      list.appendChild(d);
    });
    selectFile(files[0], list.firstChild);
  } catch { list.innerHTML = '<span class="muted">Failed to load folder.</span>'; }
}

$('btn-results-load').addEventListener('click', () => {
  const p = $('results-custom-path').value.trim();
  if (p) loadFilesFromFolder(p);
  else   loadSessions();
});

$('results-custom-path').addEventListener('keydown', e => {
  if (e.key === 'Enter') $('btn-results-load').click();
});

$('btn-results-browse').addEventListener('click', () => {
  openDirModal($('results-custom-path').value.trim() || 'results', path => {
    $('results-custom-path').value = path;
    loadFilesFromFolder(path);
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// INIT
// ═══════════════════════════════════════════════════════════════════════════

// Keep xterm sized correctly when the browser window is resized
window.addEventListener('resize', () => {
  if (_termState === 'visible') fitAddon.fit();
});

loadSysinfo();
loadConfig();
loadRunContainers();
buildExportMetrics();
renderQueue();
