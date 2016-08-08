# -*- mode: ruby -*-
Vagrant.configure("2") do |config|
    config.vm.box = "ubuntu/trusty64"

    config.vm.hostname = "local.terrarium.bettermatter.com"
    config.vm.network "private_network", ip: "192.168.25.88"

    config.vm.synced_folder ".", "/home/vagrant/bettermatter", type: "nfs"

    config.vm.provider "virtualbox" do |vb|
        vb.name = "bettermatter"
        vb.memory = "2048"
    end

    config.vm.provision "ansible" do |ansible|
        ansible.playbook = "deployment/vagrant.yml"
    end
end
