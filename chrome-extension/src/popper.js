import { createPopper } from '@popperjs/core';

/*
 * create a popper notification and attach it to the correct html dom
 */
export class Popper {

  constructor(filesInfo) {
    var arrow = document.createElement("div");
    arrow.setAttribute("id", "arrow");
    arrow.setAttribute("data-popper-arrow", "");

    var tooltipText = document.createElement("div");
    tooltipText.setAttribute("id", "tooltipText");

    var tooltip = document.createElement("pre");
    tooltip.setAttribute("id", "tooltip");
    tooltip.setAttribute("role", "tooltip");
    tooltip.appendChild(tooltipText)
    tooltip.appendChild(arrow)

    this.tooltip = tooltip;
    this.filesInfo = filesInfo;
  }

  tooltipOptions () {
    return {
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
  }

  // attach a display notification event on all code span that have type annotations
  mkNotificationEvents() {
    let popperInstance = null;
    document.querySelectorAll("span.izuna-char").forEach(span => {
      span.addEventListener("mouseover", event => {
        const typeInfo = this.filesInfo.findType(span.dataset.filePath, span.dataset.state, span.dataset.col, span.dataset.row);
        if(typeInfo) {
          var whichClassToSearch;
          if(Number.isInteger(typeInfo.centerCol)) {
            whichClassToSearch = 'izuna-char';
          } else {
            whichClassToSearch = 'izuna-fake-char';
          }
          const realSpan = document.querySelector(`span.${whichClassToSearch}[data-file-path="${span.dataset.filePath}"][data-row="${span.dataset.row}"][data-col="${typeInfo.centerCol}"]`);
          if(realSpan) {
            popperInstance = createPopper(realSpan, this.tooltip, this.tooltipOptions());
            this.tooltip.querySelector('#tooltipText').innerHTML = typeInfo.typeName;
            this.tooltip.setAttribute('data-show', '');
          }
        }
      });

      span.addEventListener("mouseleave", event => {
        this.tooltip.removeAttribute('data-show');
        if (popperInstance) {
          popperInstance.destroy();
          popperInstance = null;
        }
      });
    });
  }
}
