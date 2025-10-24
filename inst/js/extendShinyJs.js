/* Focus on element with 'id' when shiny modal is shown */
shinyjs.autoFocusModal = function(id) {
  document.getElementById('shiny-modal').addEventListener(
    'shown.bs.modal',
    () => document.getElementById(id).focus(),
    { once: true }
  );
}

/* When user has focus on 'id' they can press enter to mock a click to
 * button/link/...
 * Typically used to submit a form or trigger an action.
 */
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

/* Jump focus to element with 'id' and if it is an input / textarea, go to end
 * of the input.
 */
shinyjs.jumpToFocus = function(focus_id) {
  const input = document.getElementById(focus_id);
  input.focus();
  if (typeof input.setSelectionRange === 'function') {
    input.setSelectionRange(input.value.length, input.value.length);
  }
}

/* Enable pressing Enter anywhere in the modal to trigger a button click.
 * This listens for Enter key events on all input fields within the modal.
 * submit_id is the ID of the button to click when Enter is pressed.
 */
shinyjs.enterToSubmitModal = function(submit_id) {
  document.getElementById('shiny-modal').addEventListener(
    'shown.bs.modal',
    () => {
      const modal = document.getElementById('shiny-modal');
      modal.addEventListener('keyup', (e) => {
        if (e.key === 'Enter' && (e.target.tagName === 'INPUT' || e.target.tagName === 'SELECT')) {
          e.preventDefault();
          document.getElementById(submit_id).click();
        }
      });
    },
    { once: true }
  );
}
