FROM debian:latest

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442 && \
    echo 'deb http://download.fpcomplete.com/debian jessie main' > /etc/apt/sources.list.d/fpco.list

RUN apt-get -y update
RUN apt-get -y install zsh less git openssh-client sudo stack libcairo-dev pkg-config screen psmisc

RUN groupadd -g 1000 coercive && useradd -g coercive -m -u 1000 coercive -s /bin/bash

# Delete some cache files to make the image smaller
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# The container needs to be able to pull updates from Github, so we bundle a
# deployment SSH key
ADD deploy_key /deploy_key

USER coercive
ENV HOME /home/coercive

# pull and build once so all the packages get built
RUN cd && \
    mkdir .ssh && chmod 700 .ssh && \
    cp /deploy_key .ssh/id_rsa && \
    chmod 600 .ssh/id_rsa && \
    ssh-keyscan github.com >> .ssh/known_hosts && \
    git clone git@github.com:niklasb/coercive.git
RUN cd ~/coercive && stack setup
RUN cd ~/coercive && stack build

# update and re-build
RUN git config --global user.email coercive@coercive && \
    git config --global user.name coercive && \
    cd ~/coercive && git fetch --all && git reset --hard origin/master && stack build

# In the unlikely event that we are hacked, hopefully this will keep
# the attacker busy long enough so we can respond
USER root
ADD flag /flag
ADD gibe_flag_plx /gibe_flag_plx
RUN groupadd -g 1001 flag && \
    useradd -g flag -u 1001 flag -d'/' -s /bin/false && \
    chown root:root /flag && \
    chown root:root /gibe_flag_plx && \
    chmod 040 /flag && chmod 2555 /gibe_flag_plx

RUN echo 'coercive ALL=(flag,%flag) NOPASSWD: ALL' >> /etc/sudoers
RUN echo 'coercive ALL=NOPASSWD: /home/coercive/coercive/scripts/cleanup.sh' >> /etc/sudoers

USER coercive
RUN echo "export HISTFILE=" >> ~/.bashrc
RUN rm -f ~/.bash_history
CMD ~/coercive/scripts/restart.sh; /bin/bash
