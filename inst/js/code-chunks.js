$(document).ready(function() {
  function initializeCodeChunks() {
    $('.code-chunk-content').removeClass('show');
    $('.code-chunk-toggle').attr('aria-expanded', 'false');
  }
  
  initializeCodeChunks();
  
  $(document).on('shown.bs.modal', function() {
    setTimeout(initializeCodeChunks, 100);
  });
  
  $(document).on('click', '.code-chunk-toggle', function() {
    const targetId = $(this).attr('data-bs-target');
    const target = $(targetId);
    const isExpanded = $(this).attr('aria-expanded') === 'true';
    
    if (isExpanded) {
      target.removeClass('show');
      $(this).attr('aria-expanded', 'false');
    } else {
      target.addClass('show');
      $(this).attr('aria-expanded', 'true');
    }
  });
});
