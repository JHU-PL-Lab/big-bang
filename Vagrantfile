Vagrant.configure(2) do |config|
  config.vm.box = "ubuntu/trusty64"

  config.vm.provision :docker
  config.vm.provision :docker_compose
  config.vm.provision "shell", inline: "echo 'cd /vagrant' >> .bashrc"
end
