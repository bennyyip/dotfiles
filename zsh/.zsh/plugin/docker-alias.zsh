#
# Defines Docker aliases.
#
# Author:
#   François Vantomme <akarzim@gmail.com>
#

#
# Aliases
#

# Docker
alias dk='sudo docker'
alias dka='sudo docker attach'
alias dkb='sudo docker build'
alias dkd='sudo docker diff'
alias dkdf='sudo docker system df'
alias dke='sudo docker exec'
alias dkE='sudo docker exec -e COLUMNS=`tput cols` -e LINES=`tput lines` -i -t'
alias dkh='sudo docker history'
alias dki='sudo docker images'
alias dkin='sudo docker inspect'
alias dkim='sudo docker import'
alias dkk='sudo docker kill'
alias dkkh='sudo docker kill -s HUP'
alias dkl='sudo docker logs'
alias dkL='sudo docker logs -f'
alias dkli='sudo docker login'
alias dklo='sudo docker logout'
alias dkls='sudo docker ps'
alias dkp='sudo docker pause'
alias dkP='sudo docker unpause'
alias dkpl='sudo docker pull'
alias dkph='sudo docker push'
alias dkps='sudo docker ps'
alias dkpsa='sudo docker ps -a'
alias dkr='sudo docker run'
alias dkR='sudo docker run -e COLUMNS=`tput cols` -e LINES=`tput lines` -i -t --rm'
alias dkRe='sudo docker run -e COLUMNS=`tput cols` -e LINES=`tput lines` -i -t --rm --entrypoint /bin/bash'
alias dkRM='sudo docker system prune'
alias dkrm='sudo docker rm'
alias dkrmi='sudo docker rmi'
alias dkrn='sudo docker rename'
alias dks='sudo docker start'
alias dkS='sudo docker restart'
alias dkss='sudo docker stats'
alias dksv='sudo docker save'
alias dkt='sudo docker tag'
alias dktop='sudo docker top'
alias dkup='sudo docker update'
alias dkV='sudo docker volume'
alias dkv='sudo docker version'
alias dkw='sudo docker wait'
alias dkx='sudo docker stop'

## Container (C)
alias dkC='sudo docker container'
alias dkCa='sudo docker container attach'
alias dkCcp='sudo docker container cp'
alias dkCd='sudo docker container diff'
alias dkCe='sudo docker container exec'
alias dkCE='sudo docker container exec -e COLUMNS=`tput cols` -e LINES=`tput lines` -i -t'
alias dkCin='sudo docker container inspect'
alias dkCk='sudo docker container kill'
alias dkCl='sudo docker container logs'
alias dkCL='sudo docker container logs -f'
alias dkCls='sudo docker container ls'
alias dkCp='sudo docker container pause'
alias dkCpr='sudo docker container prune'
alias dkCrn='sudo docker container rename'
alias dkCS='sudo docker container restart'
alias dkCrm='sudo docker container rm'
alias dkCr='sudo docker container run'
alias dkCR='sudo docker container run -e COLUMNS=`tput cols` -e LINES=`tput lines` -i -t --rm'
alias dkCRe='sudo docker container run -e COLUMNS=`tput cols` -e LINES=`tput lines` -i -t --rm --entrypoint /bin/bash'
alias dkCs='sudo docker container start'
alias dkCss='sudo docker container stats'
alias dkCx='sudo docker container stop'
alias dkCtop='sudo docker container top'
alias dkCP='sudo docker container unpause'
alias dkCup='sudo docker container update'
alias dkCw='sudo docker container wait'

## Image (I)
alias dkI='sudo docker image'
alias dkIb='sudo docker image build'
alias dkIh='sudo docker image history'
alias dkIim='sudo docker image import'
alias dkIin='sudo docker image inspect'
alias dkIls='sudo docker image ls'
alias dkIpr='sudo docker image prune'
alias dkIpl='sudo docker image pull'
alias dkIph='sudo docker image push'
alias dkIrm='sudo docker image rm'
alias dkIsv='sudo docker image save'
alias dkIt='sudo docker image tag'

## Volume (V)
alias dkV='sudo docker volume'
alias dkVin='sudo docker volume inspect'
alias dkVls='sudo docker volume ls'
alias dkVpr='sudo docker volume prune'
alias dkVrm='sudo docker volume rm'

## Network (N)
alias dkN='sudo docker network'
alias dkNs='sudo docker network connect'
alias dkNx='sudo docker network disconnect'
alias dkNin='sudo docker network inspect'
alias dkNls='sudo docker network ls'
alias dkNpr='sudo docker network prune'
alias dkNrm='sudo docker network rm'

## System (Y)
alias dkY='sudo docker system'
alias dkYdf='sudo docker system df'
alias dkYpr='sudo docker system prune'

## Stack (K)
alias dkK='sudo docker stack'
alias dkKls='sudo docker stack ls'
alias dkKps='sudo docker stack ps'
alias dkKrm='sudo docker stack rm'

## Swarm (W)
alias dkW='sudo docker swarm'

## CleanUp (rm)
# Clean up exited containers (docker < 1.13)
alias dkrmC='sudo docker rm $(docker ps -qaf status=exited)'

# Clean up dangling images (docker < 1.13)
alias dkrmI='sudo docker rmi $(docker images -qf dangling=true)'

# Pull all tagged images
alias dkplI='sudo docker images --format "{{ .Repository }}" | grep -v "^<none>$" | xargs -L1 docker pull'

# Clean up dangling volumes (docker < 1.13)
alias dkrmV='sudo docker volume rm $(docker volume ls -qf dangling=true)'

# Docker Machine (m)
alias dkm='sudo docker-machine'
alias dkma='sudo docker-machine active'
alias dkmcp='sudo docker-machine scp'
alias dkmin='sudo docker-machine inspect'
alias dkmip='sudo docker-machine ip'
alias dkmk='sudo docker-machine kill'
alias dkmls='sudo docker-machine ls'
alias dkmpr='sudo docker-machine provision'
alias dkmps='sudo docker-machine ps'
alias dkmrg='sudo docker-machine regenerate-certs'
alias dkmrm='sudo docker-machine rm'
alias dkms='sudo docker-machine start'
alias dkmsh='sudo docker-machine ssh'
alias dkmst='sudo docker-machine status'
alias dkmS='sudo docker-machine restart'
alias dkmu='sudo docker-machine url'
alias dkmup='sudo docker-machine upgrade'
alias dkmv='sudo docker-machine version'
alias dkmx='sudo docker-machine stop'

# Docker Compose (c)
alias dkc='sudo docker-compose'
alias dkcb='sudo docker-compose build'
alias dkcB='sudo docker-compose build --no-cache'
alias dkcd='sudo docker-compose down'
alias dkce='sudo docker-compose exec -e COLUMNS=`tput cols` -e LINES=`tput lines`'
alias dkck='sudo docker-compose kill'
alias dkcl='sudo docker-compose logs'
alias dkcL='sudo docker-compose logs -f'
alias dkcls='sudo docker-compose ps'
alias dkcp='sudo docker-compose pause'
alias dkcP='sudo docker-compose unpause'
alias dkcpl='sudo docker-compose pull'
alias dkcph='sudo docker-compose push'
alias dkcps='sudo docker-compose ps'
alias dkcr='sudo docker-compose run -e COLUMNS=`tput cols` -e LINES=`tput lines`'
alias dkcR='sudo docker-compose run -e COLUMNS=`tput cols` -e LINES=`tput lines` --rm'
alias dkcrm='sudo docker-compose rm'
alias dkcs='sudo docker-compose start'
alias dkcsc='sudo docker-compose scale'
alias dkcS='sudo docker-compose restart'
alias dkcu='sudo docker-compose up'
alias dkcU='sudo docker-compose up -d'
alias dkcv='sudo docker-compose version'
alias dkcx='sudo docker-compose stop'
