#https://reformatcode.com/code/git/automatic-merge-branch-into-master-on-sucessful-build-in-travis

echo "merge_script.sh"
if [ "$TRAVIS_BRANCH" == "master" ]; then
    echo "merge into production; check out deep repo"
    # https://stackoverflow.com/a/32580822/1007926
    echo "git clone git@github.com:${TRAVIS_REPO_SLUG}.git"
    git clone git@github.com:${TRAVIS_REPO_SLUG}.git $TRAVIS_REPO_SLUG
    cd $TRAVIS_REPO_SLUG
    echo "git checkout -qf $TRAVIS_COMMIT"
    git checkout -qf $TRAVIS_COMMIT
    echo "git checkout production"
    git checkout production || exit
    echo "git merge $TRAVIS_COMMIT"
    git merge "$TRAVIS_COMMIT" || exit
    git push origin production || exit
fi

if [ "$TRAVIS_BRANCH" == "production" ]; then
    echo "CI build of production branch successful"
    docker login -u "$DOCKER_USERNAME" -p "$DOCKER_PASSWORD"
    echo "logged into Docker Hub"
    docker push peterbecich/stock-servant
    docker push peterbecich/stock-servant-stock-servant-local
    docker push peterbecich/stock-servant-stock-servant    
fi
