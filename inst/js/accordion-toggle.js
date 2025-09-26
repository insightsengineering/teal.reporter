// Toggle R Code accordions within a specific card
function toggleRCodeAccordions(cardId) {
  const cardElement = document.querySelector('[data-rank-id="' + cardId + '"]');
  if (!cardElement) return;
  
  // Find only R Code accordion buttons (not the main card accordion)
  const buttons = cardElement.querySelectorAll('.accordion-button');
  const rCodeButtons = Array.from(buttons).filter(btn => {
    const titleText = btn.textContent || '';
    return titleText.includes('R Code') || btn.querySelector('.fa-code, .fas.fa-code, [class*="fa-code"]');
  });
  
  if (rCodeButtons.length === 0) return;
  
  // Check if any R Code accordion is open
  const anyOpen = rCodeButtons.some(btn => 
    btn.getAttribute('aria-expanded') === 'true'
  );
  
  // Toggle only R Code accordions based on current state
  rCodeButtons.forEach(btn => {
    if (anyOpen && btn.getAttribute('aria-expanded') === 'true') {
      btn.click(); // Close if any are open
    } else if (!anyOpen && btn.getAttribute('aria-expanded') === 'false') {
      btn.click(); // Open if all are closed
    }
  });
}
