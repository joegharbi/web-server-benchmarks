'use strict';

const $ = id => document.getElementById(id);
const $$ = sel => document.querySelectorAll(sel);

// ─── Chart color palette ────────────────────────────────────────────────
const COLORS = [
  '#6366f1','#22c55e','#f59e0b','#ef4444','#06b6d4','#ec4899',
  '#8b5cf6','#14b8a6','#f97316','#3b82f6','#a855f7','#84cc16',
  '#e11d48','#0ea5e9','#d946ef','#facc15',
];

// ─── WebSocket x-axis detection (mirrors gui_graph_generator.py) ────────
const WS_XAXIS_COLUMNS = [
  'Num Clients','Message Size (KB)','Rate (msg/s)',
  'Bursts','Duration (s)','Interval (s)',
];
const XAXIS_DISPLAY = {
  'Num Clients':'Number of clients','Message Size (KB)':'Message size (KB)',
  'Rate (msg/s)':'Rate (msg/s)','Bursts':'Bursts',
  'Duration (s)':'Duration (s)','Interval (s)':'Interval (s)',
  'Total Requests':'Total requests',
};

function safeFloat(v, def = 0) {
  if (v === null || v === undefined || v === '' || v === 'NaN') return def;
  const n = parseFloat(v);
  return isNaN(n) ? def : n;
}

function detectWsSubtype(name, headers, rows) {
  const lower = (name || '').toLowerCase();
  if (lower.includes('_concurrency')) return 'concurrency';
  if (lower.includes('_payload')) return 'payload';
  if (lower.includes('_burst')) return 'burst';
  if (lower.includes('_stream')) return 'stream';
  if (!rows || !rows.length || !headers.includes('Pattern')) return null;
  const patterns = new Set(rows.map(r => (r.Pattern || '').trim().toLowerCase()));
  if (patterns.has('burst') && !patterns.has('stream')) return 'burst';
  if (patterns.has('stream')) return 'stream';
  return null;
}

function wsXaxisColumn(headers, rows, subtype) {
  if (subtype === 'concurrency' && headers.includes('Num Clients')) return 'Num Clients';
  if (subtype === 'payload' && headers.includes('Message Size (KB)')) return 'Message Size (KB)';
  for (const col of WS_XAXIS_COLUMNS) {
    if (!headers.includes(col)) continue;
    const vals = rows.map(r => safeFloat(r[col])).filter(v => v !== 0);
    if (new Set(vals).size > 1) return col;
  }
  for (const col of WS_XAXIS_COLUMNS) {
    if (headers.includes(col)) return col;
  }
  return null;
}

function getNumericColumns(headers, sampleRows) {
  // Show ALL columns that contain at least one numeric value
  const skip = new Set(['Container Name', 'Type', 'Test Type', 'Pattern', 'HTTP Max Workers']);
  return headers.filter(h => {
    if (skip.has(h)) return false;
    if (!sampleRows || !sampleRows.length) return true;  // include by default if no rows
    // Check if any value in this column parses as a number
    return sampleRows.some(r => {
      const v = r[h];
      return v !== null && v !== undefined && v !== '' && !isNaN(parseFloat(v));
    });
  });
}

function formatBytes(b) {
  if (b < 1024) return b + ' B';
  if (b < 1024 * 1024) return (b / 1024).toFixed(1) + ' KB';
  return (b / (1024 * 1024)).toFixed(1) + ' MB';
}

function formatTime(sec) {
  const h = Math.floor(sec / 3600);
  const m = Math.floor((sec % 3600) / 60);
  const s = Math.floor(sec % 60);
  return [h, m, s].map(v => String(v).padStart(2, '0')).join(':');
}

function formatDate(ts) {
  if (!ts) return '--';
  return new Date(ts * 1000).toLocaleString();
}

function typeTag(type) {
  const cls = { static: 'tag-static', dynamic: 'tag-dynamic', websocket: 'tag-websocket' };
  return `<span class="tag ${cls[type] || 'tag-unknown'}">${type}</span>`;
}

function escHtml(s) {
  const el = document.createElement('span');
  el.textContent = s;
  return el.innerHTML;
}

// ─── API helpers ────────────────────────────────────────────────────────
async function api(path, opts = {}) {
  const res = await fetch('/api/' + path, {
    headers: { 'Content-Type': 'application/json' },
    ...opts,
  });
  if (!res.ok) {
    const detail = await res.text().catch(() => res.statusText);
    throw new Error(detail);
  }
  return res.json();
}

function apiPost(path, body = {}) {
  return api(path, { method: 'POST', body: JSON.stringify(body) });
}

// ─── Flash messages ─────────────────────────────────────────────────────
function flash(tabId, msg, type = 'ok') {
  const el = $('flash-' + tabId);
  if (!el) return;
  el.className = `flash show flash-${type}`;
  el.textContent = msg;
  clearTimeout(el._timer);
  el._timer = setTimeout(() => { el.classList.remove('show'); }, 5000);
}

// ═══════════════════════════════════════════════════════════════════════
//  APP — single global namespace
// ═══════════════════════════════════════════════════════════════════════
const App = {
  // state
  containers: [],
  sessions: [],
  selectedSession: null,
  selectedFiles: [],
  csvCache: {},
  queue: [],
  queueRunning: false,
  chart: null,
  browseCallback: null,
  browseCurrent: '.',
  confirmAction: null,
  elapsedTimer: null,
  elapsedStart: 0,

  // ─── Unified paths (synced across tabs, persisted in localStorage) ──
  getBenchDir() {
    return localStorage.getItem('bench-gui-bench-dir') || 'benchmarks';
  },
  setBenchDir(v) {
    localStorage.setItem('bench-gui-bench-dir', v || 'benchmarks');
    this.syncBenchDir();
  },
  getResultsDir() {
    return localStorage.getItem('bench-gui-results-dir') || 'results';
  },
  setResultsDir(v) {
    localStorage.setItem('bench-gui-results-dir', v || 'results');
    this.syncResultsDir();
  },
  syncBenchDir() {
    const val = this.getBenchDir();
    // Sync all bench-dir inputs across tabs
    for (const id of ['env-bench-dir', 'containers-bench-dir']) {
      if ($(id)) $(id).value = val;
    }
    if ($('cs-benchdir')) $('cs-benchdir').textContent = val;
  },
  syncResultsDir() {
    const val = this.getResultsDir();
    if ($('env-results-dir')) $('env-results-dir').value = val;
    if ($('results-root-dir')) $('results-root-dir').value = val;
    if ($('cs-resultsdir')) $('cs-resultsdir').textContent = val;
  },
  initPaths() {
    this.syncBenchDir();
    this.syncResultsDir();
    // Sync on change from any input
    for (const id of ['env-bench-dir', 'containers-bench-dir']) {
      if ($(id)) $(id).addEventListener('change', () => this.setBenchDir($(id).value));
    }
    for (const id of ['env-results-dir', 'results-root-dir']) {
      if ($(id)) $(id).addEventListener('change', () => this.setResultsDir($(id).value));
    }
  },

  // ─── Init ───────────────────────────────────────────────────────────
  init() {
    this.initTheme();
    this.initPaths();
    this.initTabs();
    this.initTerminal();
    this.initTerminalResize();
    this.initTargetWatcher();
    this.initNotifyEmail();
    this.loadDashboard();
    this.pollStatus();
  },

  // ─── Theme ──────────────────────────────────────────────────────────
  initTheme() {
    const saved = localStorage.getItem('bench-gui-theme') || 'dark';
    this.applyTheme(saved);
  },

  toggleTheme() {
    const current = document.documentElement.getAttribute('data-theme') || 'dark';
    const next = current === 'dark' ? 'light' : 'dark';
    this.applyTheme(next);
    localStorage.setItem('bench-gui-theme', next);
  },

  applyTheme(theme) {
    document.documentElement.setAttribute('data-theme', theme);
    $('theme-toggle').innerHTML = theme === 'dark' ? '&#9790;' : '&#9728;';
    // Update xterm theme if terminal exists
    if (this.term) {
      const isDark = theme === 'dark';
      this.term.options.theme = {
        background: isDark ? '#0f1117' : '#ffffff',
        foreground: isDark ? '#d1d5db' : '#1f2937',
        cursor: isDark ? '#818cf8' : '#6366f1',
        selectionBackground: isDark ? 'rgba(99,102,241,0.3)' : 'rgba(99,102,241,0.2)',
      };
    }
  },

  // ─── Tabs ───────────────────────────────────────────────────────────
  initTabs() {
    for (const btn of $$('.tab-btn')) {
      btn.addEventListener('click', () => {
        $$('.tab-btn').forEach(b => b.classList.remove('active'));
        $$('.tab-panel').forEach(p => p.classList.remove('active'));
        btn.classList.add('active');
        $('tab-' + btn.dataset.tab).classList.add('active');
        this.onTabActivate(btn.dataset.tab);
      });
    }
  },

  onTabActivate(tab) {
    if (tab === 'config') this.loadConfig();
    if (tab === 'containers') this.loadContainers();
    if (tab === 'run') this.loadContainersForRun();
    if (tab === 'results') this.loadSessions();
    if (tab === 'utilities') this.loadLogs();
  },

  // ─── Terminal (xterm.js + WebSocket) ────────────────────────────────
  initTerminal() {
    const isDark = (document.documentElement.getAttribute('data-theme') || 'dark') === 'dark';
    this.term = new Terminal({
      theme: {
        background: isDark ? '#0f1117' : '#ffffff',
        foreground: isDark ? '#d1d5db' : '#1f2937',
        cursor: isDark ? '#818cf8' : '#6366f1',
        selectionBackground: isDark ? 'rgba(99,102,241,0.3)' : 'rgba(99,102,241,0.2)',
      },
      fontFamily: "'JetBrains Mono','Fira Code','Cascadia Code',monospace",
      fontSize: 13,
      cursorBlink: true,
      scrollback: 5000,
    });
    // Robust FitAddon import — handle different CDN export shapes
    const FA = window.FitAddon || window.FitAddon_ || {};
    const FitCls = FA.FitAddon || FA;
    this.fitAddon = new FitCls();
    this.term.loadAddon(this.fitAddon);
    this.term.open($('xterm-container'));
    // Re-fit terminal on window resize
    window.addEventListener('resize', () => this.termFit());

    this.term.onData(data => {
      if (this.ws && this.ws.readyState === WebSocket.OPEN) {
        this.ws.send(JSON.stringify({ type: 'input', data }));
      }
    });

    this.connectWs();
  },

  connectWs() {
    const proto = location.protocol === 'https:' ? 'wss:' : 'ws:';
    this.ws = new WebSocket(`${proto}//${location.host}/ws/terminal`);

    this.ws.onopen = () => {
      this.termFit();
    };

    this.ws.onmessage = (ev) => {
      try {
        const msg = JSON.parse(ev.data);
        if (msg.ping) return;
        if (msg.stream === 'stdout' || msg.stream === 'stderr') {
          this.term.write(msg.data);
          this.termShow();
        }
        if (msg.exit !== undefined) {
          const code = msg.exit;
          const color = code === 0 ? '\x1b[32m' : '\x1b[31m';
          this.term.write(`\r\n${color}[${msg.label || 'process'}] exited with code ${code}\x1b[0m\r\n`);
          this.onProcessExit(msg.label, code);
        }
      } catch (_) {}
    };

    this.ws.onclose = () => {
      setTimeout(() => this.connectWs(), 3000);
    };
  },

  termFit() {
    try {
      this.fitAddon.fit();
      const dims = this.fitAddon.proposeDimensions();
      if (dims && this.ws && this.ws.readyState === WebSocket.OPEN) {
        this.ws.send(JSON.stringify({ type: 'resize', rows: dims.rows, cols: dims.cols }));
      }
    } catch (_) {}
  },

  termShow() {
    const strip = $('terminal-strip');
    if (!strip.classList.contains('visible')) {
      this.termToggle('visible');
    }
  },

  termClickToggle() {
    const strip = $('terminal-strip');
    if (strip.classList.contains('visible')) {
      this.termToggle('minimized');
    } else {
      this.termToggle('visible');
    }
  },

  termToggle(state) {
    const strip = $('terminal-strip');
    strip.classList.remove('visible', 'minimized', 'hidden');
    strip.classList.add(state);
    const btn = $('t-btn-toggle');
    if (state === 'visible') {
      btn.innerHTML = '&#9660;';
      btn.title = 'Hide terminal';
      setTimeout(() => this.termFit(), 50);
    } else {
      btn.innerHTML = '&#9650;';
      btn.title = 'Show terminal';
    }
  },

  termClear() {
    this.term.clear();
  },

  initTerminalResize() {
    const handle = $('terminal-resize');
    const strip = $('terminal-strip');
    let startY, startH;

    handle.addEventListener('mousedown', (e) => {
      startY = e.clientY;
      startH = strip.offsetHeight;
      strip.style.transition = 'none';

      const onMove = (e2) => {
        const newH = Math.max(80, startH - (e2.clientY - startY));
        strip.style.height = newH + 'px';
      };
      const onUp = () => {
        strip.style.transition = '';
        document.removeEventListener('mousemove', onMove);
        document.removeEventListener('mouseup', onUp);
        this.termFit();
      };
      document.addEventListener('mousemove', onMove);
      document.addEventListener('mouseup', onUp);
    });
  },

  // ─── Target watcher (show/hide server selector) ────────────────────
  initTargetWatcher() {
    $('run-target').addEventListener('change', () => {
      const target = $('run-target').value;
      const needServer = target.includes('single');
      $('server-group').style.display = needServer ? '' : 'none';
    });
    // Auto-replot when metric or plot type changes
    $('chart-metric').addEventListener('change', () => {
      if (this.selectedFiles.length && $('chart-metric').value) this.plotChart();
    });
    $('chart-type').addEventListener('change', () => {
      if (this.selectedFiles.length && $('chart-metric').value) this.plotChart();
    });
  },

  // ─── Email notification persistence ───────────────────────────────
  initNotifyEmail() {
    const saved = localStorage.getItem('bench-gui-email') || '';
    const enabled = localStorage.getItem('bench-gui-email-enabled') === 'true';
    if ($('notify-email')) $('notify-email').value = saved;
    if ($('notify-enabled')) $('notify-enabled').checked = enabled;
    // Auto-save on change
    if ($('notify-email')) {
      $('notify-email').addEventListener('change', () => {
        localStorage.setItem('bench-gui-email', $('notify-email').value);
      });
    }
    if ($('notify-enabled')) {
      $('notify-enabled').addEventListener('change', () => {
        localStorage.setItem('bench-gui-email-enabled', $('notify-enabled').checked);
      });
    }
  },

  // ─── Status polling ────────────────────────────────────────────────
  async pollStatus() {
    try {
      const st = await api('status');
      const dotJob = $('dot-job');
      const chipText = $('chip-job-text');
      if (st.running) {
        dotJob.className = 'dot ok';
        chipText.textContent = st.label || 'Running';
        $('terminal-label').textContent = st.label || '';
        $('btn-run').disabled = true;
        $('btn-stop').disabled = false;
        $('run-status').style.display = '';
        $('run-status-label').textContent = st.label || 'Running...';
        if (!this.elapsedTimer) this.startElapsed(st.started);
      } else {
        dotJob.className = 'dot';
        chipText.textContent = 'Idle';
        $('btn-run').disabled = false;
        $('btn-stop').disabled = true;
        $('run-status').style.display = 'none';
        this.stopElapsed();
      }
    } catch (_) {}
    setTimeout(() => this.pollStatus(), 3000);
  },

  startElapsed(started) {
    this.elapsedStart = started || (Date.now() / 1000);
    this.stopElapsed();
    const tick = () => {
      const sec = Math.floor(Date.now() / 1000 - this.elapsedStart);
      $('run-elapsed').textContent = formatTime(sec);
    };
    tick();
    this.elapsedTimer = setInterval(tick, 1000);
  },

  stopElapsed() {
    if (this.elapsedTimer) {
      clearInterval(this.elapsedTimer);
      this.elapsedTimer = null;
    }
  },

  onProcessExit(label, code) {
    $('btn-run').disabled = false;
    $('btn-stop').disabled = true;
    $('run-status').style.display = 'none';
    $('terminal-label').textContent = '';
    this.stopElapsed();

    if (this.queueRunning) {
      this.advanceQueue(code);
    }
  },

  // ═══ DASHBOARD ════════════════════════════════════════════════════
  async loadDashboard() {
    try {
      const info = await api('sysinfo');

      $('si-cpu').textContent = info.cpu_model || '--';
      $('si-cores').textContent = `${info.cpu_physical}P / ${info.cpu_logical}L`;
      $('si-mem').textContent = info.memory_gb ? info.memory_gb + ' GB' : '--';
      $('si-os').textContent = info.os || '--';
      $('si-kernel').textContent = info.kernel || '--';
      $('si-host').textContent = info.hostname || '--';
      $('si-gov').textContent = info.governor || '--';
      $('si-temp').textContent = info.temp_current ? info.temp_current + ' C' : '--';

      $('chip-host-text').textContent = info.hostname || 'localhost';

      // Docker chip
      const dotDocker = $('dot-docker');
      dotDocker.className = 'dot ' + (info.docker_ok ? 'ok' : 'err');

      // Temp chip
      const dotTemp = $('dot-temp');
      const tempVal = info.temp_current;
      if (tempVal) {
        $('chip-temp-text').textContent = tempVal + ' C';
        dotTemp.className = 'dot ' + (tempVal < 60 ? 'ok' : tempVal < 80 ? 'warn' : 'err');
      }

      // Checklist
      const check = (id, ok) => {
        $(id).innerHTML = ok ? '&#9989;' : '&#10060;';
      };
      check('chk-venv', info.venv_ok);
      check('chk-docker', info.docker_ok);
      check('chk-scaph', !!info.scaphandre);
      check('chk-rapl', !!info.rapl);
      check('chk-config', info.config_exists);

      // Config summary
      $('cs-isolation').textContent = info.isolation || '--';
      $('cs-runs').textContent = info.runs || '--';
      $('cs-filter').textContent = info.filter || '--';
      $('cs-benchdir').textContent = this.getBenchDir();
      $('cs-resultsdir').textContent = this.getResultsDir();

    } catch (err) {
      flash('dashboard', 'Failed to load system info: ' + err.message, 'err');
    }

    this.loadRecentSessions();
  },

  async loadRecentSessions() {
    try {
      const resultsDir = this.getResultsDir();
      const sessions = await api('results/sessions?root=' + encodeURIComponent(resultsDir));
      const tbody = $('recent-sessions');
      if (!sessions.length) {
        tbody.innerHTML = '<tr><td colspan="5" class="empty-state" style="font-style:italic">No benchmark sessions yet. Run a benchmark to get started.</td></tr>';
        return;
      }
      tbody.innerHTML = sessions.slice(0, 10).map(s => {
        const date = s.mtime ? new Date(s.mtime * 1000).toLocaleString(undefined, {
          month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit'
        }) : '--';
        return `<tr>
          <td style="font-family:var(--mono);font-size:.78rem">${escHtml(s.name)}</td>
          <td>${(s.types || []).map(t => typeTag(t)).join(' ')}</td>
          <td>${s.csv_count}</td>
          <td style="font-size:.75rem;color:var(--text-dim)">${date}</td>
          <td><button class="btn btn-sm" onclick="App.viewSession('${escHtml(s.path)}')">View</button></td>
        </tr>`;
      }).join('');
    } catch (_) {}
  },

  viewSession(path) {
    $$('.tab-btn').forEach(b => b.classList.remove('active'));
    $$('.tab-panel').forEach(p => p.classList.remove('active'));
    $$('.tab-btn[data-tab="results"]')[0].classList.add('active');
    $('tab-results').classList.add('active');
    this.selectedSession = path;
    this.loadSessions();
    this.loadFilesForSession(path);
  },

  // ═══ CONFIGURATION ════════════════════════════════════════════════
  async loadConfig() {
    try {
      const cfg = await api('config');
      for (const [section, keys] of Object.entries(cfg)) {
        for (const [key, value] of Object.entries(keys)) {
          const el = $(`cfg-${section}-${key}`);
          if (!el) continue;
          if (el.type === 'checkbox') {
            el.checked = value === 'true' || value === true;
          } else {
            el.value = value;
          }
        }
      }
      flash('config', 'Configuration loaded', 'ok');
    } catch (err) {
      flash('config', 'Failed to load config: ' + err.message, 'err');
    }
  },

  async saveConfig() {
    const cfg = {};
    for (const el of $$('[id^="cfg-"]')) {
      const parts = el.id.replace('cfg-', '').split('-');
      const key = parts.pop();
      const section = parts.join('_');
      if (!section || !key) continue;
      if (!cfg[section]) cfg[section] = {};
      if (el.type === 'checkbox') {
        cfg[section][key] = el.checked ? 'true' : 'false';
      } else {
        cfg[section][key] = el.value;
      }
    }
    try {
      await apiPost('config', cfg);
      flash('config', 'Configuration saved', 'ok');
    } catch (err) {
      flash('config', 'Save failed: ' + err.message, 'err');
    }
  },

  async runProfiler() {
    try {
      await apiPost('profile');
      this.termShow();
      flash('config', 'Profiler started — see terminal', 'ok');
    } catch (err) {
      flash('config', 'Profiler failed: ' + err.message, 'err');
    }
  },

  async runModelSelector() {
    try {
      await apiPost('model-selector', { apply: true });
      this.termShow();
      flash('config', 'Model selector started — see terminal', 'ok');
    } catch (err) {
      flash('config', 'Model selector failed: ' + err.message, 'err');
    }
  },

  // ═══ CONTAINERS ═══════════════════════════════════════════════════
  async loadContainers() {
    const benchDir = this.getBenchDir();
    try {
      const containers = await api('containers?bench_dir=' + encodeURIComponent(benchDir));
      this.containers = containers;
      $('container-count').textContent = containers.length;

      const body = $('containers-body');
      if (!containers.length) {
        body.innerHTML = '<div class="empty-state">No containers found in benchmarks/</div>';
        return;
      }

      const groups = {};
      for (const c of containers) {
        if (!groups[c.type]) groups[c.type] = [];
        groups[c.type].push(c);
      }

      let html = '';
      for (const [type, list] of Object.entries(groups).sort()) {
        html += `<div class="container-group">`;
        html += `<div class="container-group-header">${typeTag(type)} <span>${list.length} containers</span></div>`;
        html += `<div class="container-list">`;
        for (const c of list) {
          html += `<div class="container-card">
            <div class="c-name">${escHtml(c.name)}</div>
            <div class="c-path">${escHtml(c.path)}</div>
          </div>`;
        }
        html += `</div></div>`;
      }
      body.innerHTML = html;
    } catch (err) {
      flash('containers', 'Failed to load containers: ' + err.message, 'err');
    }
  },

  async loadContainersForRun() {
    if (this.containers.length === 0) await this.loadContainers();
    const sel = $('run-server');
    sel.innerHTML = '<option value="">-- select --</option>';
    for (const c of this.containers) {
      sel.innerHTML += `<option value="${escHtml(c.name)}">${escHtml(c.name)} (${c.type})</option>`;
    }
  },

  // ═══ RUN BENCHMARKS ═══════════════════════════════════════════════
  async startRun() {
    const target = $('run-target').value;
    const server = $('run-server').value;
    const env = this.collectEnvVars();

    if (target.includes('single') && !server) {
      flash('run', 'Select a server for single-run targets', 'warn');
      return;
    }

    try {
      await apiPost('run', { target, server, env });
      this.termShow();
      this.startElapsed();
      $('btn-run').disabled = true;
      $('btn-stop').disabled = false;
      $('run-status').style.display = '';
      $('run-status-label').textContent = target + (server ? ':' + server : '');
      flash('run', `Started: make ${target}` + (server ? ` SERVER=${server}` : ''), 'ok');
    } catch (err) {
      flash('run', 'Failed to start: ' + err.message, 'err');
    }
  },

  collectEnvVars() {
    const env = {};
    const benchDir = this.getBenchDir();
    if (benchDir && benchDir !== 'benchmarks') env.BENCH_DIR = benchDir;
    const maxW = $('env-max-workers').value.trim();
    if (maxW) env.HTTP_MAX_WORKERS = maxW;
    const port = $('env-host-port').value.trim();
    if (port && port !== '8001') env.HOST_PORT = port;
    env.BENCH_MEASURE_QUIET = $('env-quiet').checked ? '1' : '0';
    const hb = $('env-heartbeat').value.trim();
    if (hb && hb !== '60') env.MEASURE_HEARTBEAT_SEC = hb;
    return env;
  },

  async stopJob() {
    try {
      await apiPost('stop');
      flash('run', 'Stop signal sent', 'ok');
    } catch (err) {
      flash('run', 'Stop failed: ' + err.message, 'err');
    }
  },

  // ─── Queue ────────────────────────────────────────────────────────
  addToQueue() {
    const target = $('run-target').value;
    const server = $('run-server').value;
    const env = this.collectEnvVars();
    const label = target + (server ? ':' + server : '');
    this.queue.push({ target, server, env, label, status: 'pending' });
    this.renderQueue();
  },

  addAllToQueue() {
    const target = $('run-target').value;
    const env = this.collectEnvVars();
    for (const c of this.containers) {
      this.queue.push({
        target: target.includes('single') ? target : 'run-single-super-quick',
        server: c.name, env, label: `run-single:${c.name}`, status: 'pending',
      });
    }
    this.renderQueue();
  },

  clearQueue() {
    this.queue = [];
    this.queueRunning = false;
    this.renderQueue();
  },

  renderQueue() {
    const list = $('queue-list');
    $('queue-count').textContent = this.queue.length;
    if (!this.queue.length) {
      list.innerHTML = '<li class="empty-state" style="font-size:.8rem">Queue is empty. Add runs above.</li>';
      return;
    }
    list.innerHTML = this.queue.map((item, i) => {
      const cls = item.status === 'running' ? 'q-running' : item.status === 'done' ? 'q-done' : '';
      const icon = item.status === 'running' ? '<span class="spinner"></span>' :
                   item.status === 'done' ? '&#9989;' :
                   item.status === 'error' ? '&#10060;' : '&#9898;';
      return `<li class="queue-item ${cls}">
        ${icon}
        <span class="q-label">${escHtml(item.label)}</span>
        <span class="q-status">${item.status}</span>
        <button class="q-remove" onclick="App.removeFromQueue(${i})" title="Remove">&#10005;</button>
      </li>`;
    }).join('');
  },

  removeFromQueue(idx) {
    this.queue.splice(idx, 1);
    this.renderQueue();
  },

  async startQueue() {
    if (this.queueRunning || !this.queue.length) return;
    this.queueRunning = true;
    this.advanceQueue(0);
  },

  async advanceQueue(lastCode) {
    const current = this.queue.find(q => q.status === 'running');
    if (current) {
      current.status = lastCode === 0 ? 'done' : 'error';
    }

    const next = this.queue.find(q => q.status === 'pending');
    if (!next) {
      this.queueRunning = false;
      this.renderQueue();
      flash('run', 'Queue finished', 'ok');
      return;
    }

    next.status = 'running';
    this.renderQueue();

    try {
      await apiPost('run', { target: next.target, server: next.server, env: next.env });
      this.termShow();
      this.startElapsed();
      $('btn-run').disabled = true;
      $('btn-stop').disabled = false;
      $('run-status').style.display = '';
      $('run-status-label').textContent = next.label;
    } catch (err) {
      next.status = 'error';
      this.renderQueue();
      setTimeout(() => this.advanceQueue(1), 500);
    }
  },

  // ═══ RESULTS & CHARTS ════════════════════════════════════════════
  async loadSessions() {
    const resultsDir = this.getResultsDir();
    try {
      const sessions = await api('results/sessions?root=' + encodeURIComponent(resultsDir));
      this.sessions = sessions;
      const list = $('session-list');
      if (!sessions.length) {
        list.innerHTML = '<div class="empty-state">No benchmark runs found</div>';
        return;
      }
      list.innerHTML = sessions.map(s => {
        const sel = this.selectedSession === s.path ? 'selected' : '';
        const types = (s.types || []).map(t => typeTag(t)).join(' ');
        const date = s.mtime ? new Date(s.mtime * 1000).toLocaleDateString(undefined, { month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit' }) : '';
        return `<div class="file-tree-item ${sel}" onclick="App.selectSession('${escHtml(s.path)}')" title="${escHtml(s.path)}">
          <span style="flex:1;display:flex;flex-direction:column;gap:2px">
            <span style="font-weight:500">${escHtml(s.name)}</span>
            <span style="font-size:.65rem;color:var(--text-dim)">${types} ${s.csv_count} files &middot; ${date}</span>
          </span>
        </div>`;
      }).join('');
    } catch (_) {}
  },

  selectSession(path) {
    this.selectedSession = path;
    this.selectedFiles = [];
    this.csvCache = {};  // clear cache on session change
    this.loadSessions();
    this.loadFilesForSession(path);
    // Reset chart and metric dropdown
    $('chart-metric').innerHTML = '<option value="">-- load files first --</option>';
    if (this.chart) { this.chart.destroy(); this.chart = null; }
    $('chart-placeholder').style.display = '';
  },

  async loadFilesForSession(path) {
    try {
      const files = await api('results/files?path=' + encodeURIComponent(path));
      this.renderFileList(files);
    } catch (err) {
      flash('results', 'Failed to load files: ' + err.message, 'err');
    }
  },

  renderFileList(files) {
    const list = $('file-list');
    this._allFiles = files;  // store for select-all
    if (!files.length) {
      list.innerHTML = '<div class="empty-state">No CSV files found</div>';
      $('file-sel-actions').style.display = 'none';
      return;
    }

    $('file-sel-actions').style.display = '';
    const groups = {};
    for (const f of files) {
      if (!groups[f.type]) groups[f.type] = [];
      groups[f.type].push(f);
    }

    let html = '';
    for (const [type, items] of Object.entries(groups).sort()) {
      html += `<div class="file-tree-group">${type} <span style="font-weight:400;opacity:.6">(${items.length})</span></div>`;
      for (const f of items) {
        const sel = this.selectedFiles.some(sf => sf.path === f.path) ? 'selected' : '';
        html += `<div class="file-tree-item ${sel}" onclick="App.toggleFile(${JSON.stringify(f).replace(/"/g, '&quot;')})">
          ${typeTag(f.type)}
          <span style="flex:1">${escHtml(f.stem)}</span>
          <span style="font-size:.68rem;color:var(--text-dim)">${formatBytes(f.size)}</span>
        </div>`;
      }
    }
    list.innerHTML = html;
  },

  selectAllFiles() {
    if (!this._allFiles) return;
    this.selectedFiles = [...this._allFiles];
    this.refreshFileListSelection();
    this.updateMetricDropdown();
  },

  deselectAllFiles() {
    this.selectedFiles = [];
    this.refreshFileListSelection();
    $('chart-metric').innerHTML = '<option value="">-- load files first --</option>';
  },

  toggleFile(file) {
    const idx = this.selectedFiles.findIndex(f => f.path === file.path);
    if (idx >= 0) {
      this.selectedFiles.splice(idx, 1);
    } else {
      this.selectedFiles.push(file);
    }
    this.refreshFileListSelection();
    this.updateMetricDropdown();
    if (this.selectedFiles.length === 1) {
      this.loadCsvTable(this.selectedFiles[0].path);
    }
  },

  refreshFileListSelection() {
    const items = $$('#file-list .file-tree-item');
    items.forEach(item => {
      const onclick = item.getAttribute('onclick');
      const isSelected = this.selectedFiles.some(f => onclick && onclick.includes(f.path));
      item.classList.toggle('selected', isSelected);
    });
    // Update selected count badge
    const badge = $('file-sel-count');
    if (badge) {
      badge.textContent = this.selectedFiles.length || '';
      badge.style.display = this.selectedFiles.length ? '' : 'none';
    }
  },

  async updateMetricDropdown() {
    const sel = $('chart-metric');
    if (!this.selectedFiles.length) {
      sel.innerHTML = '<option value="">-- load files first --</option>';
      return;
    }

    const allHeaders = new Set();
    let sampleRows = [];
    for (const f of this.selectedFiles) {
      if (!this.csvCache[f.path]) {
        try {
          this.csvCache[f.path] = await api('results/csv?path=' + encodeURIComponent(f.path));
        } catch (_) { continue; }
      }
      const data = this.csvCache[f.path];
      (data.headers || []).forEach(h => allHeaders.add(h));
      if (data.rows && data.rows.length) sampleRows = sampleRows.concat(data.rows.slice(0, 3));
    }

    const numeric = getNumericColumns([...allHeaders], sampleRows);
    const prev = sel.value;
    sel.innerHTML = numeric.map(h => `<option value="${escHtml(h)}">${escHtml(h)}</option>`).join('');
    // Restore previous selection if still available
    if (prev && numeric.includes(prev)) sel.value = prev;
  },

  async loadCsvTable(path) {
    if (!this.csvCache[path]) {
      try {
        this.csvCache[path] = await api('results/csv?path=' + encodeURIComponent(path));
      } catch (err) {
        flash('results', 'Failed to load CSV: ' + err.message, 'err');
        return;
      }
    }
    const data = this.csvCache[path];
    const headers = data.headers || [];
    this._tableRows = data.rows || [];
    this._tableHeaders = headers;
    this._tableSortCol = null;
    this._tableSortAsc = true;

    $('dt-count').textContent = this._tableRows.length + ' rows';
    this.renderTable(this._tableRows);
  },

  renderTable(rows) {
    const headers = this._tableHeaders || [];
    $('dt-head').innerHTML = '<tr>' + headers.map(h => {
      const arrow = this._tableSortCol === h ? (this._tableSortAsc ? ' &#9650;' : ' &#9660;') : '';
      return `<th style="cursor:pointer" onclick="App.sortTable('${escHtml(h)}')">${escHtml(h)}${arrow}</th>`;
    }).join('') + '</tr>';
    $('dt-body').innerHTML = rows.slice(0, 200).map(row =>
      '<tr>' + headers.map(h => `<td>${escHtml(String(row[h] ?? ''))}</td>`).join('') + '</tr>'
    ).join('');
  },

  sortTable(col) {
    if (this._tableSortCol === col) {
      this._tableSortAsc = !this._tableSortAsc;
    } else {
      this._tableSortCol = col;
      this._tableSortAsc = true;
    }
    const sorted = [...(this._tableRows || [])].sort((a, b) => {
      const va = a[col] ?? '', vb = b[col] ?? '';
      const na = parseFloat(va), nb = parseFloat(vb);
      if (!isNaN(na) && !isNaN(nb)) return this._tableSortAsc ? na - nb : nb - na;
      return this._tableSortAsc ? String(va).localeCompare(String(vb)) : String(vb).localeCompare(String(va));
    });
    this.renderTable(sorted);
  },

  // ─── Charting ─────────────────────────────────────────────────────
  async plotChart() {
    const metric = $('chart-metric').value;
    const plotType = $('chart-type').value;
    if (!metric || !this.selectedFiles.length) {
      flash('results', 'Select files and a metric first', 'warn');
      return;
    }

    // Show loading state
    $('chart-placeholder').textContent = 'Loading data...';
    $('chart-placeholder').style.display = '';

    // Ensure all CSV data is loaded
    for (const f of this.selectedFiles) {
      if (!this.csvCache[f.path]) {
        try {
          this.csvCache[f.path] = await api('results/csv?path=' + encodeURIComponent(f.path));
        } catch (_) { continue; }
      }
    }

    if (this.chart) {
      this.chart.destroy();
      this.chart = null;
    }
    $('chart-placeholder').style.display = 'none';

    const datasets = [];
    let xLabel = 'Test parameter';
    let isWebsocket = false;

    for (let i = 0; i < this.selectedFiles.length; i++) {
      const f = this.selectedFiles[i];
      const data = this.csvCache[f.path];
      if (!data) continue;
      const headers = data.headers || [];
      const rows = data.rows || [];
      const color = COLORS[i % COLORS.length];

      // Determine label
      let label = f.stem;
      if (headers.includes('Container Name') && rows.length && rows[0]['Container Name']) {
        label = rows[0]['Container Name'];
      }

      // Determine x-axis
      let xValues, yValues;
      if (f.type === 'websocket') {
        isWebsocket = true;
        const subtype = detectWsSubtype(f.name, headers, rows);
        const xcol = wsXaxisColumn(headers, rows, subtype);
        if (xcol) {
          xLabel = XAXIS_DISPLAY[xcol] || xcol;
          xValues = rows.map(r => safeFloat(r[xcol]));
        } else {
          xValues = rows.map((_, idx) => idx + 1);
        }
      } else if (headers.includes('Total Requests')) {
        xLabel = 'Total requests';
        xValues = rows.map(r => safeFloat(r['Total Requests']));
      } else {
        xValues = rows.map((_, idx) => idx + 1);
      }
      yValues = rows.map(r => safeFloat(r[metric]));

      datasets.push({
        label,
        data: xValues.map((x, idx) => ({ x, y: yValues[idx] })),
        borderColor: color,
        backgroundColor: color + '33',
        pointBackgroundColor: color,
        pointRadius: 3,
        borderWidth: 2,
        tension: 0.2,
        fill: false,
      });
    }

    if (plotType === 'heatmap') {
      this.plotHeatmap(datasets, metric, xLabel);
      return;
    }

    const chartType = plotType === 'bar' ? 'bar' : 'line';

    if (plotType === 'bar') {
      // For bar charts, group by x value
      const allX = [...new Set(datasets.flatMap(ds => ds.data.map(p => p.x)))].sort((a, b) => a - b);
      const barDatasets = datasets.map(ds => ({
        label: ds.label,
        data: allX.map(x => {
          const pt = ds.data.find(p => p.x === x);
          return pt ? pt.y : 0;
        }),
        backgroundColor: ds.borderColor + '99',
        borderColor: ds.borderColor,
        borderWidth: 1,
      }));

      this.chart = new Chart($('chart-canvas'), {
        type: 'bar',
        data: { labels: allX.map(String), datasets: barDatasets },
        options: this.chartOptions(metric, xLabel),
      });
    } else {
      this.chart = new Chart($('chart-canvas'), {
        type: 'scatter',
        data: { datasets },
        options: {
          ...this.chartOptions(metric, xLabel),
          showLine: true,
        },
      });
    }
  },

  themeColors() {
    const s = getComputedStyle(document.documentElement);
    return {
      text:    s.getPropertyValue('--text').trim()      || '#d1d5db',
      textDim: s.getPropertyValue('--text-dim').trim()   || '#8b8f98',
      textHead:s.getPropertyValue('--text-head').trim()  || '#f3f4f6',
      bgCard:  s.getPropertyValue('--bg-card').trim()    || '#181a20',
      bgInput: s.getPropertyValue('--bg-input').trim()   || '#1e2028',
      border:  s.getPropertyValue('--border').trim()     || '#2a2d35',
      borderHi:s.getPropertyValue('--border-hi').trim()  || '#3a3d48',
    };
  },

  chartOptions(metric, xLabel) {
    const tc = this.themeColors();
    return {
      responsive: true,
      maintainAspectRatio: false,
      plugins: {
        legend: {
          position: 'top',
          labels: { color: tc.text, font: { size: 11 }, boxWidth: 12 },
        },
        tooltip: {
          backgroundColor: tc.bgInput,
          titleColor: tc.textHead,
          bodyColor: tc.text,
          borderColor: tc.borderHi,
          borderWidth: 1,
        },
      },
      scales: {
        x: {
          title: { display: true, text: xLabel, color: tc.textDim },
          ticks: { color: tc.textDim },
          grid: { color: tc.border + '40' },
        },
        y: {
          title: { display: true, text: metric, color: tc.textDim },
          ticks: { color: tc.textDim },
          grid: { color: tc.border + '40' },
        },
      },
    };
  },

  plotHeatmap(datasets, metric, xLabel) {
    if (!datasets.length) return;
    const tc = this.themeColors();

    const canvas = $('chart-canvas');
    const ctx = canvas.getContext('2d');

    // Collect all unique x values and labels
    const labels = datasets.map(ds => ds.label);
    const allX = [...new Set(datasets.flatMap(ds => ds.data.map(p => p.x)))].sort((a, b) => a - b);

    // Build matrix
    const matrix = datasets.map(ds => {
      return allX.map(x => {
        const pt = ds.data.find(p => p.x === x);
        return pt ? pt.y : null;
      });
    });

    // Find min/max for color scale
    const flat = matrix.flat().filter(v => v !== null);
    const minVal = Math.min(...flat);
    const maxVal = Math.max(...flat);

    // Set canvas size
    const cellW = Math.max(40, Math.min(80, Math.floor(600 / allX.length)));
    const cellH = Math.max(28, Math.min(50, Math.floor(300 / labels.length)));
    const padL = 160, padT = 40, padR = 20, padB = 60;
    canvas.width = padL + allX.length * cellW + padR;
    canvas.height = padT + labels.length * cellH + padB;
    canvas.style.width = canvas.width + 'px';
    canvas.style.height = canvas.height + 'px';

    ctx.fillStyle = tc.bgInput;
    ctx.fillRect(0, 0, canvas.width, canvas.height);

    // Draw cells
    for (let row = 0; row < labels.length; row++) {
      for (let col = 0; col < allX.length; col++) {
        const val = matrix[row][col];
        const x = padL + col * cellW;
        const y = padT + row * cellH;

        if (val === null) {
          ctx.fillStyle = tc.border;
        } else {
          const ratio = maxVal === minVal ? 0.5 : (val - minVal) / (maxVal - minVal);
          const r = Math.round(15 + ratio * 84);
          const g = Math.round(23 + (1 - ratio) * 174);
          const b = Math.round(212 - ratio * 80);
          ctx.fillStyle = `rgb(${r},${g},${b})`;
        }
        ctx.fillRect(x + 1, y + 1, cellW - 2, cellH - 2);

        if (val !== null) {
          ctx.fillStyle = tc.textHead;
          ctx.font = '10px sans-serif';
          ctx.textAlign = 'center';
          ctx.textBaseline = 'middle';
          ctx.fillText(val.toFixed(1), x + cellW / 2, y + cellH / 2);
        }
      }
    }

    // Y labels
    ctx.fillStyle = tc.text;
    ctx.font = '11px sans-serif';
    ctx.textAlign = 'right';
    ctx.textBaseline = 'middle';
    for (let i = 0; i < labels.length; i++) {
      const text = labels[i].length > 20 ? labels[i].substring(0, 18) + '..' : labels[i];
      ctx.fillText(text, padL - 8, padT + i * cellH + cellH / 2);
    }

    // X labels
    ctx.textAlign = 'center';
    ctx.textBaseline = 'top';
    for (let i = 0; i < allX.length; i++) {
      ctx.fillText(String(allX[i]), padL + i * cellW + cellW / 2, padT + labels.length * cellH + 8);
    }

    // Axis labels
    ctx.fillStyle = tc.textDim;
    ctx.font = '12px sans-serif';
    ctx.textAlign = 'center';
    ctx.fillText(xLabel, padL + (allX.length * cellW) / 2, canvas.height - 10);

    ctx.save();
    ctx.translate(12, padT + (labels.length * cellH) / 2);
    ctx.rotate(-Math.PI / 2);
    ctx.fillText(metric, 0, 0);
    ctx.restore();

    if (this.chart) { this.chart.destroy(); this.chart = null; }
  },

  exportChart() {
    const canvas = $('chart-canvas');
    const link = document.createElement('a');
    const metric = $('chart-metric').value || 'chart';
    const slug = metric.replace(/[^\w\s-]/g, '').trim().replace(/[\s-]+/g, '-').toLowerCase();
    const ts = new Date().toISOString().slice(0, 16).replace(/[T:]/g, '-');
    link.download = `${slug}-${this.selectedFiles.length}bench-${ts}.png`;
    link.href = canvas.toDataURL('image/png');
    link.click();
  },

  async batchExport() {
    if (!this.selectedFiles.length) {
      flash('results', 'Select files first', 'warn');
      return;
    }
    const metric = $('chart-metric').value;
    if (!metric) {
      flash('results', 'Select a metric first', 'warn');
      return;
    }

    // Get all numeric metrics from selected files
    const allHeaders = new Set();
    let sampleRows = [];
    for (const f of this.selectedFiles) {
      if (!this.csvCache[f.path]) {
        try { this.csvCache[f.path] = await api('results/csv?path=' + encodeURIComponent(f.path)); }
        catch (_) { continue; }
      }
      const data = this.csvCache[f.path];
      (data.headers || []).forEach(h => allHeaders.add(h));
      if (data.rows) sampleRows = sampleRows.concat(data.rows.slice(0, 3));
    }
    const metrics = getNumericColumns([...allHeaders], sampleRows);

    const plotType = $('chart-type').value;
    let exported = 0;
    const ts = new Date().toISOString().slice(0, 16).replace(/[T:]/g, '-');

    flash('results', `Batch exporting ${metrics.length} charts...`, 'ok');

    for (const m of metrics) {
      $('chart-metric').value = m;
      await this.plotChart();
      // Small delay for render
      await new Promise(r => setTimeout(r, 100));
      const canvas = $('chart-canvas');
      const slug = m.replace(/[^\w\s-]/g, '').trim().replace(/[\s-]+/g, '-').toLowerCase();
      const link = document.createElement('a');
      link.download = `${slug}-${this.selectedFiles.length}bench-${ts}.png`;
      link.href = canvas.toDataURL('image/png');
      link.click();
      exported++;
    }

    // Restore original metric
    $('chart-metric').value = metric;
    await this.plotChart();
    flash('results', `Batch export complete: ${exported} charts downloaded`, 'ok');
  },

  async loadCustomPath() {
    const path = $('custom-results-path').value.trim();
    if (!path) return;
    try {
      const files = await api('results/files?path=' + encodeURIComponent(path));
      this.renderFileList(files);
      flash('results', `Loaded ${files.length} files from custom path`, 'ok');
    } catch (err) {
      flash('results', 'Failed to load path: ' + err.message, 'err');
    }
  },

  // ─── Browse modal ─────────────────────────────────────────────────
  browseForField(fieldId) {
    this.browseCallback = (path) => {
      $(fieldId).value = path;
      $(fieldId).dispatchEvent(new Event('change'));
    };
    const current = $(fieldId) ? $(fieldId).value || '.' : '.';
    this.openBrowse(current || '.');
  },

  browseForResults() {
    this.browseCallback = (path) => {
      $('custom-results-path').value = path;
    };
    this.openBrowse('.');
  },

  async openBrowse(path) {
    try {
      const data = await api('browse?path=' + encodeURIComponent(path));
      this.browseCurrent = data.path;
      $('browse-path').textContent = data.path;

      let html = '';
      if (data.parent) {
        html += `<div class="browse-item is-dir" onclick="App.openBrowse('${escHtml(data.parent)}')">
          <span class="b-icon">&#128194;</span>
          <span class="b-name">..</span>
        </div>`;
      }
      for (const entry of data.entries) {
        if (entry.type === 'dir') {
          html += `<div class="browse-item is-dir" onclick="App.openBrowse('${escHtml(entry.path)}')">
            <span class="b-icon">&#128194;</span>
            <span class="b-name">${escHtml(entry.name)}</span>
          </div>`;
        } else {
          html += `<div class="browse-item">
            <span class="b-icon">&#128196;</span>
            <span class="b-name">${escHtml(entry.name)}</span>
            <span style="font-size:.7rem;color:var(--text-dim)">${entry.size ? formatBytes(entry.size) : ''}</span>
          </div>`;
        }
      }
      $('browse-body').innerHTML = html;
      $('browse-modal').classList.add('open');
    } catch (err) {
      flash('results', 'Browse failed: ' + err.message, 'err');
    }
  },

  browseSelect() {
    if (this.browseCallback) {
      this.browseCallback(this.browseCurrent);
    }
    this.closeBrowse();
  },

  closeBrowse() {
    $('browse-modal').classList.remove('open');
  },

  // ═══ UTILITIES ════════════════════════════════════════════════════
  async quickAction(target) {
    try {
      await apiPost('run', { target });
      this.termShow();
      flash('dashboard', `Started: make ${target}`, 'ok');
      flash('utilities', `Started: make ${target}`, 'ok');
    } catch (err) {
      const msg = 'Failed: ' + err.message;
      flash('dashboard', msg, 'err');
      flash('utilities', msg, 'err');
    }
  },

  async sudoAuth() {
    try {
      await apiPost('sudo-auth');
      this.termShow();
      flash('dashboard', 'Sudo auth started — enter password in terminal', 'ok');
    } catch (err) {
      flash('dashboard', 'Sudo auth failed: ' + err.message, 'err');
    }
  },

  async cleanPort() {
    const port = $('clean-port-input').value || '8001';
    try {
      await apiPost('run', { target: 'clean-port', env: { PORT: port } });
      this.termShow();
      flash('utilities', `Started: make clean-port PORT=${port}`, 'ok');
    } catch (err) {
      flash('utilities', 'Failed: ' + err.message, 'err');
    }
  },

  confirmClean(target) {
    this.confirmAction = target;
    $('confirm-title').textContent = 'Confirm: make ' + target;
    const msgs = {
      'clean-benchmarks': 'This will remove the benchmarks/ folder. This cannot be undone.',
      'clean-nuclear': 'This will remove results, Docker images, and benchmarks/. This cannot be undone.',
      'clean-repo': 'This will run git clean -xfd and git reset --hard. All uncommitted work will be lost.',
    };
    $('confirm-msg').textContent = msgs[target] || 'Are you sure you want to run make ' + target + '?';
    $('confirm-modal').classList.add('open');
  },

  closeConfirm() {
    $('confirm-modal').classList.remove('open');
    this.confirmAction = null;
  },

  async confirmOk() {
    const target = this.confirmAction;
    this.closeConfirm();
    if (!target) return;

    const env = {};
    if (target === 'clean-benchmarks' || target === 'clean-nuclear') {
      env.CONFIRM = '1';
    }
    try {
      await apiPost('run', { target, env });
      this.termShow();
      flash('utilities', `Started: make ${target}`, 'ok');
    } catch (err) {
      flash('utilities', 'Failed: ' + err.message, 'err');
    }
  },

  // ─── Logs ─────────────────────────────────────────────────────────
  async loadLogs() {
    try {
      const logs = await api('logs');
      const list = $('log-list');
      if (!logs.length) {
        list.innerHTML = '<div class="empty-state">No logs found</div>';
        return;
      }
      list.innerHTML = logs.map(l => `
        <div class="log-item" onclick="App.viewLog('${escHtml(l.path)}','${escHtml(l.name)}')">
          <span class="log-name">${escHtml(l.name)}</span>
          <span class="log-meta">${formatBytes(l.size)}</span>
          <span class="log-meta">${formatDate(l.mtime)}</span>
        </div>
      `).join('');
    } catch (_) {}
  },

  async viewLog(path, name) {
    try {
      const data = await api('logs/content?path=' + encodeURIComponent(path));
      $('log-viewer-card').style.display = '';
      $('log-viewer-title').textContent = name;
      $('log-viewer-content').textContent = (data.lines || []).join('');
      // Scroll to log viewer
      $('log-viewer-card').scrollIntoView({ behavior: 'smooth', block: 'nearest' });
    } catch (err) {
      flash('utilities', 'Failed to load log: ' + err.message, 'err');
    }
  },
};

// ─── Boot ──────────────────────────────────────────────────────────────
document.addEventListener('DOMContentLoaded', () => App.init());

// Close modals on Escape
document.addEventListener('keydown', (e) => {
  if (e.key === 'Escape') {
    App.closeBrowse();
    App.closeConfirm();
  }
});
