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
function generateElmAppForRow(response, githubGeneratedRow) {
  const lineNumberDom = githubGeneratedRow.querySelector('td.blob-num:nth-child(2)');
  const lineState = lineNumberDom.classList.contains("blob-num-addition") ? "ADDED" : "DELETED";
  const lineNumber = parseInt(lineNumberDom.dataset.lineNumber) - 1;  // github starts line count at 1
  const node = githubGeneratedRow.querySelector('td.blob-code');

  Elm.Main.init({ flags:
                  {
                    lineDom: response["src/Lib.hs"]["fileContent"][lineNumber],
                    lineState: lineState
                  },
                  node: node
                });
}

chrome.runtime.sendMessage({ greeting: "hello" }, response => {
  createPopper();

  githubGeneratedLines = document
    .querySelectorAll("div[data-file-type='.hs']")[0]
    .querySelector('tbody')
    .querySelectorAll('tr[data-hunk]');

  Array.from(githubGeneratedLines).forEach (githubGeneratedRow => generateElmAppForRow(response, githubGeneratedRow));

  mkNotificationEvents();

});
