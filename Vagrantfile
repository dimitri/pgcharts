# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure("2") do |config|
  config.vm.box = "wheezy64"

  config.vm.provision :file do |file|
    file.source      = 'vm-conf/devscripts'
    file.destination = '/home/vagrant/.devscripts'
  end

  config.vm.provision :file do |file|
    file.source      = 'vm-conf/gpg.conf'
    file.destination = '/home/vagrant/.gnupg/gpg.conf'
  end

  config.vm.provision "shell" do |s|
    s.path = "vm-conf/bootstrap.sh"
    s.privileged = false
  end

  config.vm.network :forwarded_port, guest: 9042, host: 9042
end
