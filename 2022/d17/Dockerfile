FROM archlinux

RUN echo '%wheel ALL=(ALL:ALL) NOPASSWD: ALL' >> /etc/sudoers
RUN useradd -ms /bin/fish -g wheel gubsey
RUN mkdir /app

RUN pacman -Syu --noconfirm sudo helix base-devel git github-cli
RUN pacman -S --noconfirm rust rust-analyzer fish 

USER gubsey
WORKDIR /app

RUN mkdir -p ~/.config/helix
RUN printf "theme = \"tokyonight\"\n\n[editor]\ntrue-color = true" >> ~/.config/helix/config.toml

CMD [ "fish" ]