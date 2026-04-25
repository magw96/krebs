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
})();
