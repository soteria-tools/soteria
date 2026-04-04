set -e
../standalone.exe -impl $1 -o out.ml
ocamlformat --enable-outside-detected-project --impl out.ml
set +e
ocamlfind ocamlc -package soteria -c prelude.ml out.ml 2> out.err
# if ok, print "Success"
if [ $? -ne 0 ]; then
  cat out.err
  exit 1
else
  echo "Success ✅"
fi
