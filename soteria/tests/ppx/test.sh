set -e
../standalone.exe -impl $1 -o out.ml
ocamlformat --enable-outside-detected-project --impl out.ml
set +e

if [ -f "./prelude.ml" ]; then
  ocamlfind ocamlc -package soteria -c ./prelude.ml out.ml 2> out.err
else
  ocamlfind ocamlc -package soteria -c out.ml 2> out.err
fi

if [ $? -ne 0 ]; then
  cat out.err
  exit 1
else
  echo "Success ✅"
fi
