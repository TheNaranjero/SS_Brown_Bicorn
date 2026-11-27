# Actualizar la api:

correr las siguientes lineas en la VM

```

cd ~/SS_Brown_Bicorn
git pull
sudo docker build -t sampling_simulator .
sudo docker stop ixe
sudo docker rm ixe
sudo docker run -d --name ixe -p 3838:3838 --restart unless-stopped sampling_simulator

```


ip:

http://34.58.27.185:3838/
34.58.27.185.3838:3838