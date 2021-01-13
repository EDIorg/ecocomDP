# Configuration for workflows. These are loaded at the beginning of some 
# workflow functions if they exist in the global environment.

config.repository <- "" # Repository system. Must be one of the supported repositories.
config.environment <- "" # Repository environment. Some repositories have development and production environments.
config.path <- "" # Path to directory containing workflow files
config.www <- "" # Public URL for repository access to data and metadata files
config.user.id <- "" # User ID for upload to repository
config.user.pass <- "" # User password for upload to repository
config.email.address <- "" # Gmail address. Only Gmail is currently supported
config.email.pass <- "" # Password for email address