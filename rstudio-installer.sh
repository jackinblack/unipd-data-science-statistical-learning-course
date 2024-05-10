#! /bin/bash

sudo apt install -y libobjc-11-dev libclang1-14 libclang-14-dev libclang-common-14-dev libllvm14 libobjc4 libclang-dev r-base
wget https://download1.rstudio.org/electron/jammy/amd64/rstudio-2023.12.1-402-amd64.deb
sudo dpkg -i rstudio-2023.12.1-402-amd64.deb
rm -f rstudio-2023.12.1-402-amd64.deb