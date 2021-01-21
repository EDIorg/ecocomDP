# Global variables for controlling API methods and other remotes in ecocomDP functions. Primarily supporting routine_handler() and associated sub-processes.

config.repository <- "" # Repository system. Options: EDI
config.environment <- "" # Repository environment. config.repository can have multiple environments. Options: staging, production.
config.path <- "" # Path to directory where outputs are written
config.www <- "" # URL to config.path through which config.repository can download files
config.user.id <- "" # User ID for config.repository
config.user.pass <- "" # User password for config.repository
config.email.address <- "" # Gmail address for notifications (only Gmail is currently supported)
config.email.pass <- "" # Password for config.email.address