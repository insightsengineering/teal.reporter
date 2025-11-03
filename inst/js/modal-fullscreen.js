/**
 * Toggle fullscreen mode for modal dialogs
 */
function toggleModalFullscreen() {
  // Find the currently visible modal
  var modals = document.querySelectorAll('.modal.show');
  if (modals.length === 0) {
    console.warn("No visible modal found");
    return;
  }
  
  // Get the last (topmost) modal if multiple are open
  var modal = modals[modals.length - 1];
  var modalDialog = modal.querySelector('.modal-dialog');
  
  if (!modalDialog) {
    console.warn("Modal dialog not found");
    return;
  }
  
  // Find the fullscreen button
  var fullscreenBtn = modal.querySelector('.modal-fullscreen-btn');
  var icon = fullscreenBtn ? fullscreenBtn.querySelector('i') : null;
  
  // Toggle the fullscreen class
  modalDialog.classList.toggle('modal-fullscreen');
  
  // Update icon if button exists
  if (icon) {
    if (modalDialog.classList.contains('modal-fullscreen')) {
      icon.className = 'fa fa-compress';
    } else {
      icon.className = 'fa fa-expand';
    }
  }
}

