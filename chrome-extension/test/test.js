import { equal } from 'assert';
import { Splitter } from '../src/splitter.js';
import { JSDOM } from 'jsdom';

describe('Splitter', function() {
  describe('#split()', function() {
    it('foo', function() {
      const dom = new JSDOM(`<span class="whatever" data-code-marker="+">hey</span>`).window.document.querySelector("span");

      const splitter = new Splitter(new JSDOM().window.document, new JSDOM().window.Node);
      const splitted = splitter.split(JSDOM.fragment(), dom, 1)

      /*
      splitter.show(dom)
      console.log("\n###\n");
      splitter.show(splitted)
      */

      equal(true, true);
    });

    it('foo', function() {
      const dom = new JSDOM(`<span class="blob-code-inner blob-code-marker" data-code-marker="+"><span class="pl-en x x-first x-last">hey</span> <span class="pl-k">::</span> <span class="pl-en"><span class="pl-c1">String</span></span></span>`).window.document.querySelector("span");

      const splitter = new Splitter(new JSDOM().window.document, new JSDOM().window.Node);
      const splitted = splitter.split(JSDOM.fragment(), dom, 1)
      /*
      splitter.show(dom, "")
      console.log("\n###\n");
      splitter.show(splitted, "")
      */

      equal(true, true);
    });
  });
});
