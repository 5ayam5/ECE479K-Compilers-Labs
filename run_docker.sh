function ece479k_docker() {
    lab=$1
    image=$2
    if [ -z "$lab" ]; then
        echo "Usage: ece479k_docker <lab> <image>"
        return
    fi
    if [ -z "$image" ]; then
        echo "Usage: ece479k_docker <lab> <image>"
        return
    fi
    set -x
    docker run --platform linux/amd64 -it -v "$PWD/$lab":/code $image
    set +x
}
