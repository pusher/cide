module CIDE
  dir = File.expand_path('..', __FILE__)
  DOCKERFILE = 'Dockerfile.cide'
  DEFAULT_CIDEFILE = File.join(dir, 'default_cide.yml')
  TEMP_SSH_KEY = 'id_rsa.tmp'
  SSH_CONFIG_FILE = 'ssh_config'
  SSH_CONFIG_PATH = File.join(dir, SSH_CONFIG_FILE)
  CONFIG_FILES = ['cide.yml', '.cide.yml']
  CIDE_DIR = '/cide'
  CIDE_SRC_DIR = File.join(CIDE_DIR, 'src')
  CIDE_SSH_DIR = File.join(CIDE_DIR, '.ssh')
end
