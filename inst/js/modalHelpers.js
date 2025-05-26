shinyjs.autoFocusModal = function(id) {
  document.getElementById('shiny-modal').addEventListener(
    'shown.bs.modal',
    () => document.getElementById(id).focus(),
    { once: true }
  );
}

shinyjs.enterToSubmit = function(id, submit_id) {
  document.getElementById('shiny-modal').addEventListener(
    'shown.bs.modal',
    () => document.getElementById(id).addEventListener('keyup', (e) => {
      if (e.key === 'Enter') {
        e.preventDefault(); // prevent form submission
        document.getElementById(submit_id).click();
      }
    })
  );
}

shinyjs.jumpToFocus = function(focus_id) {
  const input = document.getElementById(focus_id);
  input.focus();
  input.setSelectionRange(input.value.length, input.value.length);
}
