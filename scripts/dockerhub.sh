if [ "$TRAVIS_BRANCH" == "production" ]; then
    echo "push to DockerHub"
    docker login -u "$DOCKER_USERNAME" -p "$DOCKER_PASSWORD"
    echo "logged into Docker Hub"
    echo "docker push peterbecich/stock-servant-stock-servant"
    docker push peterbecich/stock-servant-stock-servant
    echo "docker push peterbecich/stock-servant-stock-servant-local"
    docker push peterbecich/stock-servant-stock-servant-local
fi
