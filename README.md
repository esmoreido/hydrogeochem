# Инструкция
> Собираем образ на сервере

0. Установить [Docker](https://docs.docker.com/engine/install/ubuntu/) если еще нет

1. Проект использует виртуальную среду `renv`, в которой записаны источники и версии библиотек. В случае установки новых пакетов или обновлений их, надо обновить файл `renv.lock` запустив команду `renv::snapshot()`. Именно этот файл в дальнейшем используется для установки зависимостей Docker. См. подробнее [тут](https://www.appsilon.com/post/renv-with-docker).

2. В файле `Dockerfile` и `app.R` явно прописан порт **8180** на котором работает приложение. Если нужно обновить их, то обновить необходимо в обоих местах.

3. Чтобы собрать образ надо из корня проекта запустить:
```shell
# hydrogeochem -- это название образа
sudo docker build -t hydrogeochem . 
```

4. Чтобы запустить приложение:
```shell
sudo docker run -p 8180:8180 hydrogeochem
```

5. Убить образ:
```shell
# Определить ID
sudo docker ps
#> CONTAINER ID   IMAGE          COMMAND                  CREATED          STATUS          PORTS                                                   NAMES
#> f070850440df   hydrogeochem   "/bin/sh -c 'Rscript…"   51 seconds ago   Up 50 seconds   3838/tcp, 0.0.0.0:8180->8180/tcp, [::]:8180->8180/tcp   reverent_sinoussi

sudo docker stop f070850440df
```

# Альтернативная инструкция
> Копируем образ на сервер, не тратя время на сборку образа там

0. Собираем образ локально запуская
```shell
# hydrogeochem -- это название образа
sudo docker build -t hydrogeochem . 
```

1. Архивируем образ
```shell
sudo docker save hydrogeochem -o hydrogeochem.tar

# Проверяем разрешения копирования-записи
sudo chmod 644 hydrogeochem.tar
```

2. Копируем на сервер
```shell
scp hydrogeochem.tar root@vm736626.vps.masterhost.tech:/root/Projects/
```

3. Запускаем образ (на сервере)
```shell
cd /root/Projects/
docker load -i hydrogeochem.tar
```

4. Запускаем приложение
```shell
sudo docker run -p 8180:8180 hydrogeochem
```

