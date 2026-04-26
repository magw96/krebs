// Krebs V0.2 - keyboard shortcuts.
// Ctrl/Cmd + S  -> click the submit button on the active tab
// Ctrl/Cmd + N  -> open the "Registrar paciente" sidebar tab
// Esc           -> close any open bs4Dash collapsible panel that has focus
//
// We look for any visible button with class .krebs-submit-btn (added to the
// sticky submit container's button by mod_register_new and
// mod_followup_search) and trigger a click on the first one inside the
// active tab pane.

(function () {
  function activeTabRoot() {
    return document.querySelector(".tab-pane.active") || document.body;
  }

  function clickFirstVisible(selector, root) {
    var nodes = (root || activeTabRoot()).querySelectorAll(selector);
    for (var i = 0; i < nodes.length; i++) {
      var n = nodes[i];
      if (n.offsetParent !== null && !n.disabled) {
        n.click();
        n.classList.add("kbd-flash");
        setTimeout(function () { n.classList.remove("kbd-flash"); }, 350);
        return true;
      }
    }
    return false;
  }

  document.addEventListener("keydown", function (e) {
    var mod = e.ctrlKey || e.metaKey;

    // Ctrl/Cmd + S -> submit
    if (mod && (e.key === "s" || e.key === "S")) {
      // Allow native save inside textareas if the user really wants it
      if (document.activeElement &&
          document.activeElement.tagName === "TEXTAREA" && e.shiftKey) return;
      e.preventDefault();
      clickFirstVisible(".krebs-submit-btn");
    }

    // Ctrl/Cmd + N -> jump to Registrar tab
    if (mod && (e.key === "n" || e.key === "N")) {
      e.preventDefault();
      var link = document.querySelector(
        '.sidebar-menu a[data-value="tab_register"], ' +
        'a[href="#shiny-tab-tab_register"]');
      if (link) link.click();
    }

    // Ctrl/Cmd + K -> focus navbar quick-search
    if (mod && (e.key === "k" || e.key === "K")) {
      e.preventDefault();
      var qs = document.querySelector(
        ".krebs-quicksearch .selectize-input input");
      if (qs) {
        qs.focus();
        // selectize hides the real input; trigger a click on its wrapper
        // to open the dropdown so the user can type immediately.
        var wrap = document.querySelector(
          ".krebs-quicksearch .selectize-control");
        if (wrap) wrap.click();
      }
    }

    // Esc -> blur active selectize/picker dropdown
    if (e.key === "Escape") {
      var dd = document.querySelector(".dropdown-menu.show, .selectize-dropdown");
      if (dd && dd.parentElement) {
        var trg = dd.parentElement.querySelector(
          "button.dropdown-toggle, .selectize-input");
        if (trg) trg.blur();
      }
    }
  }, true);

  // ---- Bootstrap tooltips (BS4 jQuery + BS5 vanilla) ------------------------
  // bs4Dash ships Bootstrap 4 -> tooltips are jQuery plugins, NOT
  // window.bootstrap.Tooltip. We try the jQuery path first (works for BS4
  // and BS5 if jQuery shim is loaded) and fall back to the vanilla BS5
  // constructor. Re-scan after Shiny re-renders modules.
  function initTooltips() {
    if (window.jQuery && typeof window.jQuery.fn.tooltip === "function") {
      try {
        window.jQuery('[data-toggle="tooltip"], [data-bs-toggle="tooltip"]')
          .tooltip({ html: true, container: "body" });
        return;
      } catch (e) { /* fall through */ }
    }
    var bs = window.bootstrap;
    if (bs && bs.Tooltip) {
      document.querySelectorAll('[data-bs-toggle="tooltip"], [data-toggle="tooltip"]')
        .forEach(function (el) {
          if (!el._kbTooltip) {
            el._kbTooltip = new bs.Tooltip(el, { html: true, container: "body" });
          }
        });
    }
  }
  document.addEventListener("DOMContentLoaded", initTooltips);
  if (window.$) {
    $(document).on("shiny:value shiny:bound shiny:visualchange", function () {
      setTimeout(initTooltips, 50);
    });
  }
})();
