# heroku-docker-r-example-app

## Running this app locally using RStudio
Running this app with RStudio requires no additional changes. You can use RStudio to install the library dependencies for this app.
This application was built using R 3.6.1.

## Running this app locally using Docker
* Install Docker for your operating system
  - [Mac](https://docs.docker.com/docker-for-mac/install/)
  - [Windows](https://docs.docker.com/docker-for-windows/install/)
* Clone this repository
```git clone https://github.com/hmdc/heroku-docker-r-example-app.git```
* cd into the cloned repository
```cd heroku-docker-r-example-app```
* Build the container
```docker build . -t heroku-docker-r-example-app```
* Run the container
```docker run -e PORT=8080 -p 8080:8080 heroku-docker-r-example-app```
* View the app locally in your web browser at <http://localhost:8080>

## Deploying this application to Heroku
1. Log into Heroku
```heroku login```
2. ```heroku create --stack=container my-docker-r-example-app --team=g-harvard```
   * Replace ```my-docker-r-example-app``` with whatever name. ```---team``` is not necessary for non-IQSS/HMDC users.
3. ```heroku git:remote -a my-docker-r-example-app```
4. ```git push heroku master```
5. Visit the app - the app's url will be different based on what you've named it, but follows this template: <https://APPNAME.herokuapp.com>. For a working example, see <https://docker-r-example-app.herokuapp.com/>

