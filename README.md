# RBudgeting

Skeleton Shiny application built with [{golem}](https://github.com/ThinkR-open/golem), [{bs4Dash}](https://rinterface.github.io/bs4Dash/), and [{shinymanager}](https://github.com/datastorm-open/shinymanager). The project demonstrates how to integrate PostgreSQL backed role-based access control (RBAC), user management, and dark/light themes.

## Features

- PostgreSQL schema installer with connection testing
- Administrator bootstrap helper
- Authentication and session management via shinymanager
- User administration module (create, activate/deactivate, delete)
- bs4Dash layout with sidebar, navbar, and notifications placeholder
- Theme toggle supporting light and dark modes

## Development

1. Install package dependencies:

```r
install.packages(c(
  "golem", "bs4Dash", "shinymanager", "shinyWidgets", "shinyFeedback",
  "RPostgres", "DBI", "DT", "dplyr", "config", "shinyjs"
))
```

2. Configure the database credentials in `inst/golem-config.yml`.

3. Launch the app during development:

```r
pkgload::load_all(export_all = FALSE)
run_app()
```

4. Use the **Setup** tab to test connections, install the schema, and create the first administrator.

## Deployment

For production environments, edit the `production` section of `inst/golem-config.yml` and start the app via:

```r
RBudgeting::run_app()
```

The skeleton expects a PostgreSQL database accessible with the provided credentials. After creating the administrator, use the **Users** tab to manage accounts.
