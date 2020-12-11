/*
  create a popper notification to be used later on
*/
function createPopper () {
  arrow = document.createElement("div");
  arrow.setAttribute("id", "arrow");
  arrow.setAttribute("data-popper-arrow", "");

  tooltipText = document.createElement("div");
  tooltipText.setAttribute("id", "tooltipText");

  tooltip = document.createElement("pre");
  tooltip.setAttribute("id", "tooltip");
  tooltip.setAttribute("role", "tooltip");
  tooltip.appendChild(tooltipText)
  tooltip.appendChild(arrow)
  document.body.appendChild(tooltip);
}

/*
  attach a display notification event on all code span that have annotations
*/
function mkNotificationEvents() {
  const tooltip = document.querySelector('#tooltip');
  let popperInstance = null;

  document.querySelectorAll("span[data-specialized-type]").forEach(span => {
    span.addEventListener("mouseover", event => {
      const tooltipOptions = {
        placement: 'top',
        modifiers: [
          {
            name: 'offset',
            options: {
              offset: [0, 8],
            },
          },
        ],
      };
      popperInstance = Popper.createPopper(span, tooltip, tooltipOptions);
      tooltip.querySelector('#tooltipText').innerHTML = span.dataset.specializedType.replaceAll(" -> ", " ⟶ ");
      tooltip.setAttribute('data-show', '');
    });

    span.addEventListener("mouseleave", event => {
      tooltip.removeAttribute('data-show');
      if (popperInstance) {
        popperInstance.destroy();
        popperInstance = null;
      }
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
