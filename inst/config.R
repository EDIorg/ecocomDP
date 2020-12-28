# Configuration for workflows. These are loaded at the beginning of some 
# workflow functions if they exist in the global environment.

config.repository <- "EDI" # Repository system. Must be one of the supported repositories.
config.environment <- "staging" # Repository environment. Some repositories have development and production environments.
config.path <- "C:/some/dir" # Path to directory containing workflow files
config.www <- "https://some/url" # Public URL for repository access to data and metadata files
config.user.id <- "id" # User ID for upload to repository
config.user.pass <- "password" # User password for upload to repository