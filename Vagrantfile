# -*- mode: ruby -*-
# vi: set ft=ruby :

VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.box = "ubuntu/trusty64"

  config.vm.network "forwarded_port", guest: 3000, host: 3030

  project="novproject-api"

  config.vm.provider "virtualbox" do |vb|
    vb.customize [
      "modifyvm", :id, 
      "--name", "Haskell VM - #{project}",
      "--memory", "1024"
    ]
  end

  config.vm.provision "shell", inline: <<SCRIPT
  apt-get update
  apt-get upgrade
  apt-get install -y postgres-9.3 libpq-dev haskell-platform
SCRIPT

  config.vm.provision "shell", privileged: false, inline: <<SCRIPT
  echo 'export PATH="/vagrant/.cabal-sandbox/bin:/home/vagrant/yesod-sandbox/.cabal-sandbox/bin:/home/vagrant/.cabal/bin:$PATH"' >> /home/vagrant/.bashrc
    echo 'export PATH="/vagrant/.cabal-sandbox/bin:/home/vagrant/yesod-sandbox/.cabal-sandbox/bin:/home/vagrant/.cabal/bin:$PATH"' >> /home/vagrant/.profile
SCRIPT

  config.vm.provision "shell", privileged: false, inline: <<SCRIPT
  cabal update
  cabal install cabal-install
  cabal install alex yesod-platform yesod-bin
SCRIPT

  config.vm.provision "shell", privileged: false, inline: <<SCRIPT
  cd /vagrant
  cabal install
SCRIPT
end

