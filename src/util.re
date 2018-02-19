let listRange = countDown => {
  let rec addToList = (list, countDown) =>
    if (countDown <= 0) {
      list;
    } else {
      addToList([countDown, ...list], countDown - 1);
    };
  addToList([], countDown);
};
