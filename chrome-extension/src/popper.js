import { createPopper } from '@popperjs/core';
import { Constants } from './constants.js';
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

  buildSpanAttr(span, typeInfo) {
    return {
      classAttr: Number.isInteger(typeInfo.centerCol) ? Splitter.REAL_SPAN : Splitter.FAKE_SPAN,
      filePathAttr: `[data-file-path="${span.dataset.filePath}"]`,
      rowAttr: `[data-row="${span.dataset.row}"]`,
      colAttr: `[data-col="${typeInfo.centerCol}"]`,
      locationAttr: `[data-location="${span.dataset.location}"]`,
      stateAttr: `[data-state="${span.dataset.state}"]`,
    }
  }

  // attach a display notification event on all code span that have type annotations
  mkNotificationEvents(diffDom) {
    let popperInstance = null;
    diffDom.querySelectorAll(`span.${Splitter.REAL_SPAN}`).forEach(span => {
      span.addEventListener("mouseover", event => {
        const typeInfo = this.filesInfo.findType(span.dataset.filePath, span.dataset.state, span.dataset.col, span.dataset.row);
        if(typeInfo) {
          const spanAttr = this.buildSpanAttr(span, typeInfo);
          const realSpan = diffDom.querySelector(`span.${spanAttr.classAttr}${spanAttr.filePathAttr}${spanAttr.rowAttr}${spanAttr.colAttr}${spanAttr.locationAttr}${spanAttr.stateAttr}`);
          if(realSpan) {
            this.highlightFocusedRegion(diffDom, spanAttr, typeInfo);
            popperInstance = createPopper(realSpan, this.tooltip, this.tooltipOptions());
            this.tooltip.querySelector('#tooltipText').innerHTML = typeInfo.typeName;
            this.tooltip.setAttribute('data-show', '');
          }
        }
      });
      span.addEventListener("mouseleave", event => {
        const typeInfo = this.filesInfo.findType(span.dataset.filePath, span.dataset.state, span.dataset.col, span.dataset.row);
        if(typeInfo) {
          this.tooltip.removeAttribute('data-show');
          if (popperInstance) {
            popperInstance.destroy();
            popperInstance = null;
          }
          document.querySelectorAll(`span.${Constants.IZUNA_HIGHLIGHT_REGION}.${Splitter.REAL_SPAN}`).forEach(span =>
            span.classList.remove(Constants.IZUNA_HIGHLIGHT_REGION)
          );
        }
      });
    });
  }

  highlightFocusedRegion(diffDom, spanAttr, typeInfo) {
    var spansToHighlight = [];
    var i = typeInfo.startCol;
    while(i <= typeInfo.endCol) {
      const dom = diffDom.querySelector(`span.${Splitter.REAL_SPAN}${spanAttr.filePathAttr}${spanAttr.rowAttr}${spanAttr.locationAttr}${spanAttr.stateAttr}[data-col="${i}"]`);
      if(dom) {
        spansToHighlight.push(dom);
      }
      i = i + 1;
    }
    spansToHighlight.forEach(span => {
      span.classList.add(Constants.IZUNA_HIGHLIGHT_REGION);
    });
  }
}
