/*
  create a popper notification to be used later on
*/
function createPopper () {
  arrow = document.createElement("div");
  arrow.setAttribute("id", "arrow");
  arrow.setAttribute("data-popper-arrow", "");

  tooltip = document.createElement("pre");
  tooltip.setAttribute("id", "tooltip");
  tooltip.setAttribute("role", "tooltip");
  tooltip.appendChild(arrow)
  document.body.appendChild(tooltip);
}

/*
  attach a display notification event on all code span that have annotations
*/
function mkNotificationEvents() {
  document.querySelectorAll("span[data-specialized-type]").forEach(span => {
    span.addEventListener("mouseover", event => {
      Popper.createPopper(span, tooltip, { placement: 'top' });
      tooltip.innerHTML = span.dataset.specializedType.replaceAll(" -> ", " ⟶ ");
      tooltip.setAttribute('data-show', '');
    });

    span.addEventListener("mouseleave", event => {
      tooltip.removeAttribute('data-show');
    });
  });
}

/*
  make an elm application for a given row that will handle the display
*/
function generateElmAppForRow(moduleInfo, githubGeneratedRow) {
  const lineNumberDom = githubGeneratedRow.querySelector('td.blob-num:nth-child(2)');
  if(lineNumberDom.classList.contains("blob-num-addition")) {
    lineState = "ADDED";
  } else if(lineNumberDom.classList.contains("blob-num-deletion")) {
    lineState = "DELETED";
  } else {
    lineState = "UNMODIFIED"
  }
  const lineNumber = parseInt(lineNumberDom.dataset.lineNumber) - 1;  // github starts line count at 1
  const node = githubGeneratedRow.querySelector('td.blob-code');

  console.log(lineNumber + 1);
  console.log(lineState);
  console.log(githubGeneratedRow);
  console.log(moduleInfo["fileContent"][lineNumber]);

  Elm.Main.init({ flags:
                  {
                    lineDom: moduleInfo["fileContent"][lineNumber],
                    lineState: lineState
                  },
                  node: node
                });
}

chrome.runtime.sendMessage({ greeting: "hello" }, response => {
  console.log(response);
  createPopper();

  document.querySelectorAll("div.file[data-file-type='.hs']").forEach(haskellFile => {
    const src = haskellFile.querySelector(".file-header .file-info a[title]").text;
    const moduleInfo = response[src];
    if(moduleInfo) {
      Array.from(haskellFile.querySelector('tbody').querySelectorAll('tr[data-hunk]')).forEach (githubGeneratedRow => {
        generateElmAppForRow(moduleInfo, githubGeneratedRow)
      });
    }
  });

  mkNotificationEvents();
});
