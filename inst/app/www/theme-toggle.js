Shiny.addCustomMessageHandler('toggle-theme', function(message) {
  const body = document.body;
  const mode = message.mode || 'light';
  body.classList.remove('light-mode', 'dark-mode');
  body.classList.add(mode === 'dark' ? 'dark-mode' : 'light-mode');
});
