let listToTbl = list => {
  let listLen = List.length(list);
  let tbl = Hashtbl.create(listLen > 0 ? listLen : 1);
  List.iter(((key, item)) => Hashtbl.add(tbl, key, item), list);
  tbl;
};

let listRange = countDown => {
  let rec addToList = (list, countDown) =>
    if (countDown <= 0) {
      list;
    } else {
      addToList([countDown, ...list], countDown - 1);
    };
  addToList([], countDown);
};

let isSome = (o : option('a)) =>
  switch o {
  | Some(_) => true
  | None => false
  };