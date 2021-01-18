import { createPopper } from '@popperjs/core';

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

  // attach a display notification event on all code span that have type annotations
  mkNotificationEvents() {
    let popperInstance = null;

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

    document.querySelectorAll("span.izuna-char").forEach(span => {
      span.addEventListener("mouseover", event => {
        popperInstance = createPopper(span, this.tooltip, tooltipOptions);
        const typeName = this.filesInfo.findType(span.dataset.filePath, span.dataset.state, span.dataset.col, span.dataset.row);
        if(typeName) {
          this.tooltip.querySelector('#tooltipText').innerHTML = typeName;
          this.tooltip.setAttribute('data-show', '');
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
