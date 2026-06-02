/* charts.js — benchmark result visualization */

let _chart = null;

// ---------------------------------------------------------------------------
// Colour palettes
// ---------------------------------------------------------------------------

// 12-color palette with good perceptual separation
const SERIES_PALETTE = [
  '#4f8ef7', '#3ecf8e', '#f5c842', '#e05252', '#a78bfa',
  '#f07e3c', '#5ecbcb', '#c45cb4', '#91cf65', '#e88b4f',
  '#60a5fa', '#34d399',
];

// Type-based colors for bar charts grouped by type
const TYPE_COLORS = {
  'static':    { base: '#4f8ef7', bg: '#4f8ef7cc' },
  'dynamic':   { base: '#3ecf8e', bg: '#3ecf8ecc' },
  'websocket': { base: '#a78bfa', bg: '#a78bfacc' },
};

function getTypeColor(type) {
  return TYPE_COLORS[(type || '').toLowerCase()] || { base: '#8892a4', bg: '#8892a4cc' };
}

function getSeriesColor(idx) {
  return SERIES_PALETTE[idx % SERIES_PALETTE.length];
}

// Read a live CSS variable (respects dark/light theme)
function cssVar(name) {
  return getComputedStyle(document.documentElement).getPropertyValue(name).trim() || null;
}

function getTheme() {
  return {
    textColor:  cssVar('--text')   || '#e2e8f0',
    mutedColor: cssVar('--muted')  || '#8892a4',
    gridColor:  cssVar('--border') || '#2e3148',
    surface2:   cssVar('--surface2') || '#232635',
  };
}

// ---------------------------------------------------------------------------
// Custom error-bar plugin for Chart.js v4
// Each dataset must expose:
//   dataset.ciLo  — array of lower-bound values (parallel to dataset.data)
//   dataset.ciHi  — array of upper-bound values
// ---------------------------------------------------------------------------
const errorBarPlugin = {
  id: 'benchErrorBars',
  afterDatasetsDraw(chart) {
    const { ctx, scales: { y } } = chart;
    if (!y) return;
    chart.data.datasets.forEach((ds, di) => {
      if (!ds.ciLo || !ds.ciHi) return;
      const meta = chart.getDatasetMeta(di);
      if (meta.hidden) return;
      meta.data.forEach((bar, i) => {
        const lo = parseFloat(ds.ciLo[i]);
        const hi = parseFloat(ds.ciHi[i]);
        if (isNaN(lo) || isNaN(hi) || lo === hi) return;
        const x    = bar.x;
        const yLo  = y.getPixelForValue(lo);
        const yHi  = y.getPixelForValue(hi);
        const capW = Math.max(4, Math.min(10, (bar.width || 20) * 0.25));
        ctx.save();
        ctx.strokeStyle = ds.borderColor || '#888';
        ctx.lineWidth   = 1.5;
        ctx.lineCap     = 'round';
        ctx.beginPath();
        ctx.moveTo(x, yLo); ctx.lineTo(x, yHi);
        ctx.moveTo(x - capW, yLo); ctx.lineTo(x + capW, yLo);
        ctx.moveTo(x - capW, yHi); ctx.lineTo(x + capW, yHi);
        ctx.stroke();
        ctx.restore();
      });
    });
  },
};

Chart.register(errorBarPlugin);

// ---------------------------------------------------------------------------
// Main render function
// ---------------------------------------------------------------------------

/**
 * renderResultChart(rows, metricKey, chartType, showCi, offCanvas)
 *
 * rows       — flat array of CSV row objects
 * metricKey  — optional metric override (else reads DOM)
 * chartType  — optional chart type override
 * showCi     — optional CI override
 * offCanvas  — optional off-screen canvas for export (skips DOM canvas)
 *
 * Returns the Chart instance (useful for batch export).
 */
function renderResultChart(rows, metricKey, chartType, showCi, offCanvas) {
  metricKey = metricKey || document.getElementById('results-metric')?.value || 'Total Energy (J)';
  chartType = chartType || document.getElementById('results-chart-type')?.value || 'bar';
  showCi    = showCi    != null ? showCi : (document.getElementById('results-ci')?.checked ?? true);

  if (!rows?.length) return;

  // Paper style: monochrome palette override
  const isPaper = window._exportStyle === 'paper';

  // Use off-screen canvas for export, otherwise the DOM canvas
  const canvas = offCanvas || document.getElementById('results-chart');
  const ctx    = canvas.getContext('2d');

  // Only destroy the on-screen chart (not export canvases)
  if (!offCanvas && _chart) { _chart.destroy(); _chart = null; }

  const theme = offCanvas
    ? { textColor: isPaper ? '#111' : '#e2e8f0', mutedColor: isPaper ? '#444' : '#8892a4', gridColor: isPaper ? '#ccc' : '#2e3148' }
    : getTheme();

  let chart;
  if (chartType === 'bar')          chart = buildBarChart(ctx, rows, metricKey, showCi, theme, isPaper);
  else if (chartType === 'line')    chart = buildLineChart(ctx, rows, metricKey, theme, isPaper);
  else if (chartType === 'heatmap') chart = buildHeatmap(ctx, rows, metricKey, theme);

  if (!offCanvas) _chart = chart;
  return chart;
}

// ---------------------------------------------------------------------------
// Bar chart: one bar group per container, colored by type
// ---------------------------------------------------------------------------
// Paper palette: high-contrast greyscale + hatching-friendly fills
const PAPER_PALETTE = ['#000','#555','#999','#333','#777','#aaa','#222','#666','#bbb','#444'];

function buildBarChart(ctx, rows, metricKey, showCi, theme, isPaper) {
  const { textColor, mutedColor, gridColor } = theme;
  const ciLoKey = 'Energy CI Lo (J)';
  const ciHiKey = 'Energy CI Hi (J)';
  const hasCi   = showCi && rows.some(r => !isNaN(parseFloat(r[ciLoKey])));

  // One bar per unique container; color by type
  const containers = uniqueOrdered(rows, 'Container Name');

  const data  = [];
  const ciLo  = [];
  const ciHi  = [];
  const bgColors  = [];
  const bdrColors = [];

  containers.forEach(name => {
    const matching = rows.filter(r => norm(r['Container Name']) === norm(name));
    const vals     = matching.map(r => parseFloat(r[metricKey])).filter(ok);
    const los      = matching.map(r => parseFloat(r[ciLoKey])).filter(ok);
    const his      = matching.map(r => parseFloat(r[ciHiKey])).filter(ok);
    data.push(vals.length ? avg(vals) : null);
    ciLo.push(los.length ? avg(los) : null);
    ciHi.push(his.length ? avg(his) : null);
    // Determine color (paper = greyscale, colorful = by type)
    if (isPaper) {
      const pc = PAPER_PALETTE[containers.indexOf(name) % PAPER_PALETTE.length];
      bgColors.push(pc + '99');
      bdrColors.push(pc);
    } else {
      const type = (matching[0]?.['Type'] || '').toLowerCase();
      const col  = getTypeColor(type);
      bgColors.push(col.bg);
      bdrColors.push(col.base);
    }
  });

  const ds = {
    label: metricKey, data,
    backgroundColor: bgColors, borderColor: bdrColors,
    borderWidth: 1, borderRadius: 4, borderSkipped: false,
  };
  if (hasCi) { ds.ciLo = ciLo; ds.ciHi = ciHi; }

  return new Chart(ctx, {
    type: 'bar',
    data: { labels: containers, datasets: [ds] },
    options: {
      responsive: true, maintainAspectRatio: false,
      plugins: {
        legend: { display: false },
        tooltip: {
          callbacks: {
            label(ctx) {
              const v = ctx.parsed.y;
              let s   = `${metricKey}: ${v != null ? v.toFixed(4) : '—'}`;
              if (hasCi && ds.ciLo && ds.ciHi) {
                const lo = ds.ciLo[ctx.dataIndex];
                const hi = ds.ciHi[ctx.dataIndex];
                if (lo != null && hi != null)
                  s += `  CI [${parseFloat(lo).toFixed(4)} – ${parseFloat(hi).toFixed(4)}]`;
              }
              return s;
            },
            afterLabel(ctx) {
              const name = ctx.label;
              const row  = rows.find(r => norm(r['Container Name']) === norm(name));
              return row ? `Type: ${row['Type'] || '—'}` : '';
            },
          },
        },
      },
      scales: {
        x: {
          ticks: { color: mutedColor, font: { size: 10 }, maxRotation: 40 },
          grid:  { color: gridColor },
        },
        y: {
          beginAtZero: true,
          ticks: { color: mutedColor, font: { size: 10 } },
          grid:  { color: gridColor },
          title: { display: true, text: metricKey, color: mutedColor, font: { size: 11 } },
        },
      },
    },
  });
}

// ---------------------------------------------------------------------------
// Line chart: x = Total Requests, one line per container
// ---------------------------------------------------------------------------
function buildLineChart(ctx, rows, metricKey, theme, isPaper) {
  const { textColor, mutedColor, gridColor } = theme;
  const serKey   = 'Container Name';
  const serNames = uniqueOrdered(rows, serKey);

  // Point shapes for paper mode (so lines are distinguishable without colour)
  const POINT_STYLES = ['circle','triangle','rect','star','rectRot','crossRot','cross','dash'];

  const datasets = serNames.map((ser, si) => {
    const color      = isPaper ? PAPER_PALETTE[si % PAPER_PALETTE.length] : getSeriesColor(si);
    const pointStyle = POINT_STYLES[si % POINT_STYLES.length];
    const subset = rows
      .filter(r => norm(r[serKey]) === norm(ser))
      .map(r => ({ x: parseFloat(r['Total Requests']), y: parseFloat(r[metricKey]) }))
      .filter(p => !isNaN(p.x) && !isNaN(p.y))
      .sort((a, b) => a.x - b.x);
    return {
      label: ser, data: subset,
      borderColor: color, backgroundColor: color + '33',
      borderWidth: isPaper ? 1.5 : 2,
      pointRadius: 4, pointHoverRadius: 6,
      pointStyle, tension: 0.2, fill: false,
    };
  });

  return new Chart(ctx, {
    type: 'line',
    data: { datasets },
    options: {
      responsive: true, maintainAspectRatio: false,
      plugins: {
        legend: { labels: { color: textColor, font: { size: 11 }, padding: 14 } },
        tooltip: {
          callbacks: {
            label: ctx => `${ctx.dataset.label}: ${ctx.parsed.y?.toFixed(4)}`,
          },
        },
      },
      scales: {
        x: {
          type: 'linear',
          ticks: { color: mutedColor, font: { size: 10 } },
          grid:  { color: gridColor },
          title: { display: true, text: 'Total Requests', color: mutedColor, font: { size: 11 } },
        },
        y: {
          beginAtZero: true,
          ticks: { color: mutedColor, font: { size: 10 } },
          grid:  { color: gridColor },
          title: { display: true, text: metricKey, color: mutedColor, font: { size: 11 } },
        },
      },
    },
  });
}

// ---------------------------------------------------------------------------
// Heatmap: x = container names, y = request counts, color = metric value
// Uses Chart.js matrix plugin (falls back to bar if unavailable)
// ---------------------------------------------------------------------------
function buildHeatmap(ctx, rows, metricKey, theme) {
  const { textColor, mutedColor, gridColor } = theme;

  // Collect unique containers (x) and request counts (y)
  const containers = uniqueOrdered(rows, 'Container Name');
  const reqCounts  = [...new Set(rows.map(r => parseFloat(r['Total Requests'])).filter(ok))].sort((a, b) => a - b);

  // Build a map: container+requests → metric value
  const cellMap = new Map();
  rows.forEach(r => {
    const key = `${norm(r['Container Name'])}__${parseFloat(r['Total Requests'])}`;
    const val = parseFloat(r[metricKey]);
    if (!isNaN(val)) cellMap.set(key, val);
  });

  // Collect all values for color scaling
  const allVals = [...cellMap.values()];
  const minVal  = Math.min(...allVals);
  const maxVal  = Math.max(...allVals);

  function valToColor(v) {
    if (maxVal === minVal) return 'rgba(79,142,247,0.7)';
    const t = (v - minVal) / (maxVal - minVal); // 0..1 (low = blue, high = red)
    const r = Math.round(t * 220 + (1 - t) * 41);
    const g = Math.round((1 - t) * 180 * 0.5);
    const b = Math.round((1 - t) * 247 + t * 50);
    return `rgba(${r},${g},${b},0.85)`;
  }

  // Build datasets: one dataset per request count (row in heatmap)
  // We'll render as a grouped bar chart rotated to simulate heatmap
  // Each dataset = one request count level, bars = containers
  if (reqCounts.length <= 1 || containers.length <= 1) {
    // Fall back to bar chart
    return buildBarChart(ctx, rows, metricKey, false, theme);
  }

  // True heatmap using a custom matrix approach via scatter chart
  // Each point: x = container index, y = request count, color = metric
  const points = [];
  containers.forEach((cname, ci) => {
    reqCounts.forEach(req => {
      const key = `${norm(cname)}__${req}`;
      const val = cellMap.get(key);
      if (val != null) {
        points.push({ x: ci, y: req, v: val, cname, color: valToColor(val) });
      }
    });
  });

  // Use a bubble chart to simulate the heatmap cells
  const datasets = [{
    label: metricKey,
    data: points.map(p => ({
      x: p.x,
      y: p.y,
      r: 14,      // bubble radius (cell size approximation)
      v: p.v,
      cname: p.cname,
    })),
    backgroundColor: points.map(p => p.color),
    borderColor:     points.map(p => p.color),
    borderWidth: 1,
  }];

  return new Chart(ctx, {
    type: 'bubble',
    data: { datasets },
    options: {
      responsive: true, maintainAspectRatio: false,
      plugins: {
        legend: { display: false },
        tooltip: {
          callbacks: {
            label(ctx) {
              const d = ctx.raw;
              return [
                `Container: ${d.cname || containers[d.x]}`,
                `Requests: ${d.y.toLocaleString()}`,
                `${metricKey}: ${d.v?.toFixed(4)}`,
              ];
            },
          },
        },
        title: {
          display: true,
          text: `Heatmap — ${metricKey}  (blue = low, red = high)`,
          color: mutedColor, font: { size: 11 },
        },
      },
      scales: {
        x: {
          type: 'linear',
          min: -0.5,
          max: containers.length - 0.5,
          ticks: {
            color: mutedColor, font: { size: 10 },
            callback: (v) => {
              const i = Math.round(v);
              return i >= 0 && i < containers.length ? containers[i] : '';
            },
            stepSize: 1,
            maxRotation: 40,
          },
          grid: { color: gridColor },
          title: { display: true, text: 'Container', color: mutedColor, font: { size: 11 } },
        },
        y: {
          type: 'linear',
          ticks: { color: mutedColor, font: { size: 10 } },
          grid:  { color: gridColor },
          title: { display: true, text: 'Total Requests', color: mutedColor, font: { size: 11 } },
        },
      },
    },
  });
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------
function norm(v)    { return (v || '').toString().trim(); }
function ok(v)      { return !isNaN(v) && v != null; }
function avg(arr)   { return arr.reduce((a, b) => a + b, 0) / arr.length; }

function uniqueOrdered(rows, key) {
  const seen = new Set(); const out = [];
  for (const r of rows) {
    const v = norm(r[key]);
    if (!seen.has(v)) { seen.add(v); out.push(v); }
  }
  return out;
}

window.renderResultChart = renderResultChart;
