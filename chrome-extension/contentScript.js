chrome.runtime.sendMessage({ greeting: "hello" }, response => {
  console.log(response);

  arrow = document.createElement("div");
  arrow.setAttribute("id", "arrow");
  arrow.setAttribute("data-popper-arrow", "");

  tooltip = document.createElement("div");
  tooltip.setAttribute("id", "tooltip");
  tooltip.setAttribute("role", "tooltip");
  //  tooltip.innerHTML = "hello there!";
  typeInfo = response["src/Lib.hs"]["fileContent"][15];
  //console.log(typeInfo[15]);
  //const toShow = typeInfo.demangledOccName + " " + typeInfo.externalId
  const toShow = "coucou";
  tooltip.innerHTML = toShow

  tooltip.appendChild(arrow)
  document.body.appendChild(tooltip);
  console.log(1);
  line = document
    .querySelectorAll("div[data-file-type='.hs']")[0]
    .querySelector('tbody')
    .children[7]
    .querySelector('td.blob-code')
    .children[0]
    .children[2];

  lines = document
    .querySelectorAll("div[data-file-type='.hs']")[0]
    .querySelector('tbody')
    .children[15]
    .querySelectorAll('td.blob-code');

  const parser = new DOMParser();
  newDom = parser.parseFromString(typeInfo, "text/html")

  lines.forEach(line => {
    Array.from(line.children).forEach (child => child.remove());
    line.appendChild(newDom.documentElement);
  });

  lines.forEach(line => {
    line.addEventListener("mouseover", event => {
      Popper.createPopper(line, tooltip, { placement: 'top' });
      tooltip.setAttribute('data-show', '');
    });

    line.addEventListener("mouseleave", event => {
      tooltip.removeAttribute('data-show');
    });
  });

  /*
  line.addEventListener("mouseover", event => {
    //  Popper.createPopper(line, tooltip, { placement: 'top' });
    Popper.createPopper(line, tooltip, {
      modifiers: [
        {
          name: 'offset',
          options: {
            offset: [0, 8],
          },
        },
      ],
    });
    tooltip.setAttribute('data-show', '');
  });

  line.addEventListener("mouseleave", event => {
    tooltip.removeAttribute('data-show');
  });
  */
});
