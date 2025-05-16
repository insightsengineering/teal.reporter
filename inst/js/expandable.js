document.addEventListener('shown.bs.modal', function() {
  document.querySelectorAll('.expandable-container').forEach(function(container) {
    const content = container.querySelector('.expandable-content');

    if (content.scrollHeight > 500) {
      const btn = document.createElement('button');
      const gen_label = (config) => `<span><i class="fa-solid fa-${config.fa}"></i> ${config.label}</span>`;
      const more_html = gen_label({ label: 'Show more', fa: 'up-right-and-down-left-from-center'});
      const less_html = gen_label({ label: 'Show less', fa: 'down-left-and-up-right-to-center'});

      btn.innerHTML = more_html;
      btn.className = 'expand-toggle btn btn-default action-button';

      btn.addEventListener('click', function() {
        const isExpanded = content.classList.toggle('expanded');
        btn.innerHTML = isExpanded ? less_html : more_html;
      });

      container.appendChild(btn);
    }
  });
});
