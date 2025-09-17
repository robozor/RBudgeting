# First-run setup regression checklist

This manual checklist covers the unauthenticated setup path and verifies that secured content remains protected after login changes.

1. Start the RBudgeting application with a clean database (no stored credentials).
2. Load the app in a browser without authenticating. Confirm that the public setup screen is visible and that the dashboard navigation is hidden.
3. Use the setup tools to validate the database connection or install the schema as needed.
4. Create or confirm an administrator account using the setup card.
5. Sign in with the administrator credentials. Verify that the setup screen disappears and the dashboard loads automatically.
6. Confirm that the sidebar navigation is now visible and that protected tabs (Dashboard, Users) are accessible only after authentication.
7. Log out and ensure that the dashboard hides again while the setup screen returns for unauthenticated users.

Document any deviations from the expected behaviour before releasing changes.
