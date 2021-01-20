import { createPopper } from '@popperjs/core';
import { Splitter } from './splitter.js';

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
  mkNotificationEvents(diffDom) {
    let popperInstance = null;
    diffDom.querySelectorAll(`span.${Splitter.REAL_SPAN}`).forEach(span => {
      span.addEventListener("mouseover", event => {
        const typeInfo = this.filesInfo.findType(span.dataset.filePath, span.dataset.state, span.dataset.col, span.dataset.row);
        if(typeInfo) {
          const classAttr = Number.isInteger(typeInfo.centerCol) ? Splitter.REAL_SPAN : Splitter.FAKE_SPAN;
          const filePathAttr = `[data-file-path="${span.dataset.filePath}"]`;
          const rowAttr = `[data-row="${span.dataset.row}"]`;
          const colAttr = `[data-col="${typeInfo.centerCol}"]`;
          const stateAttr = `[data-state="${span.dataset.state}"]`;
          const realSpan = diffDom.querySelector(`span.${classAttr}${filePathAttr}${rowAttr}${colAttr}${stateAttr}`);
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
