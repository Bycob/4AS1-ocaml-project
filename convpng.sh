if [ $# -eq 0 ]
then
  echo "Usage : $0 [files.dot...]"
  exit
fi

for file in "$@"
do
  dot -Tpng $file > $file.png
done
