FROM ubuntu:19.04

RUN apt update && \
    apt upgrade -y && \
    apt install sudo curl -y

RUN adduser --gecos '' --disabled-password jscm && \
	  echo "jscm ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers.d/nopasswd

USER jscm
RUN mkdir -p /home/jscm/workspace
WORKDIR /home/jscm/workspace

COPY --chown=jscm . .

RUN curl -sSL https://get.haskellstack.org/ | sh
ENV PATH $PATH:/home/jscm/.local/bin

RUN stack setup && \
    stack install

ENTRYPOINT [ "jscm" ]
