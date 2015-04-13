module CIDE
	DIR = File.expand_path('..', __FILE__)
  DOCKERFILE = 'Dockerfile'
  TEMP_SSH_KEY = 'id_rsa.tmp'
  SSH_CONFIG_FILE = 'ssh_config'
  SSH_CONFIG_PATH = File.join(DIR, 'cide', SSH_CONFIG_FILE)
  CONFIG_FILE = '.cide.yml'
  CIDE_DIR = '/cide'
  CIDE_SRC_DIR = File.join(CIDE_DIR, '/src')
  CIDE_SSH_DIR = File.join(CIDE_DIR, '/.ssh')
end