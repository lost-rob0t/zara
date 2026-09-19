(() => {
  if (globalThis.__zaraBrowserContentInstalled) {
    return;
  }
  globalThis.__zaraBrowserContentInstalled = true;

  const ext = globalThis.browser ?? globalThis.chrome;
  const DEFAULT_MAX_CHARS = 20000;
  const MAX_CHARS = 100000;
  const MAX_SELECTOR_CHARS = 2048;
  const MAX_TYPED_CHARS = 100000;
  const MAX_ITEMS = 500;

  function boundedChars(value, fallback = DEFAULT_MAX_CHARS) {
    const number = Number(value ?? fallback);
    if (!Number.isInteger(number) || number < 1 || number > MAX_CHARS) {
      throw new Error(`max_chars must be between 1 and ${MAX_CHARS}`);
    }
    return number;
  }

  function selector(value) {
    if (typeof value !== "string") {
      throw new Error("selector must be a string");
    }
    const normalized = value.trim();
    if (!normalized || normalized.length > MAX_SELECTOR_CHARS) {
      throw new Error(`selector must contain 1 to ${MAX_SELECTOR_CHARS} characters`);
    }
    return normalized;
  }

  function selectedElement(value) {
    const query = selector(value);
    const element = document.querySelector(query);
    if (!element) {
      throw new Error("selector did not match an element");
    }
    return element;
  }

  function visibleText(maxChars) {
    const text = document.body?.innerText || "";
    return text.slice(0, maxChars);
  }

  function elementAttributes(element) {
    const attributes = {};
    for (const attribute of Array.from(element.attributes || []).slice(0, 32)) {
      attributes[attribute.name] = String(attribute.value).slice(0, 1000);
    }
    return attributes;
  }

  function setNativeValue(element, value) {
    let prototype = element;
    let descriptor = null;
    while (prototype && !descriptor) {
      prototype = Object.getPrototypeOf(prototype);
      descriptor = prototype && Object.getOwnPropertyDescriptor(prototype, "value");
    }
    if (descriptor?.set) {
      descriptor.set.call(element, value);
    } else {
      element.value = value;
    }
  }

  function cssPath(element) {
    if (element.id) {
      return `#${CSS.escape(element.id)}`;
    }

    const parts = [];
    let node = element;
    while (node && node.nodeType === Node.ELEMENT_NODE && parts.length < 8) {
      let part = node.localName;
      if (!part) {
        break;
      }

      if (node === document.body) {
        parts.unshift("body");
        break;
      }

      const parent = node.parentElement;
      if (parent) {
        const siblings = Array.from(parent.children).filter(
          (candidate) => candidate.localName === node.localName
        );
        if (siblings.length > 1) {
          const index = siblings.indexOf(node) + 1;
          part += `:nth-of-type(${index})`;
        }
      }

      parts.unshift(part);
      node = parent;
    }

    return parts.join(" > ");
  }

  function isVisible(element) {
    const style = globalThis.getComputedStyle(element);
    if (style.display === "none" || style.visibility === "hidden") {
      return false;
    }
    const rect = element.getBoundingClientRect();
    return rect.width > 0 && rect.height > 0;
  }

  function boundedItems(value) {
    const number = Number(value ?? 100);
    if (!Number.isInteger(number) || number < 1 || number > MAX_ITEMS) {
      throw new Error(`max_items must be between 1 and ${MAX_ITEMS}`);
    }
    return number;
  }

  function compactText(value, max = 300) {
    return String(value || "").replace(/\s+/g, " ").trim().slice(0, max);
  }

  function interactiveElementView(element) {
    const label = element.labels?.length
      ? compactText(element.labels[0].innerText || element.labels[0].textContent)
      : "";
    return {
      selector: cssPath(element),
      tag: element.tagName.toLowerCase(),
      type: compactText(element.getAttribute("type"), 80),
      name: compactText(element.getAttribute("name"), 200),
      role: compactText(element.getAttribute("role"), 80),
      text: compactText(element.innerText || element.textContent),
      label,
      placeholder: compactText(element.getAttribute("placeholder")),
      aria_label: compactText(element.getAttribute("aria-label")),
      disabled: Boolean(element.disabled || element.getAttribute("aria-disabled") === "true"),
    };
  }

  function elementsPage(args) {
    const maxItems = boundedItems(args.max_items);
    const candidates = Array.from(
      document.querySelectorAll(
        [
          "a[href]",
          "button",
          "input",
          "textarea",
          "select",
          "[role='button']",
          "[role='link']",
          "[contenteditable='true']",
        ].join(",")
      )
    );

    const elements = [];
    for (const element of candidates) {
      if (!isVisible(element)) {
        continue;
      }
      elements.push(interactiveElementView(element));
      if (elements.length >= maxItems) {
        break;
      }
    }

    return {
      url: location.href,
      count: elements.length,
      truncated: elements.length >= maxItems,
      elements,
    };
  }

  function readPage(args) {
    const maxChars = boundedChars(args.max_chars);
    const description =
      document.querySelector('meta[name="description"]')?.getAttribute("content") || "";
    const selection = String(globalThis.getSelection?.() || "").slice(0, maxChars);
    return {
      url: location.href,
      title: document.title || "",
      description: description.slice(0, 2000),
      selection,
      text: visibleText(maxChars),
    };
  }

  function extractPage(args) {
    const element = selectedElement(args.selector);
    const maxChars = boundedChars(args.max_chars, 10000);
    return {
      url: location.href,
      selector: selector(args.selector),
      tag: element.tagName.toLowerCase(),
      text: String(element.innerText ?? element.textContent ?? "").slice(0, maxChars),
      attributes: elementAttributes(element),
    };
  }

  function clickPage(args) {
    const element = selectedElement(args.selector);
    element.scrollIntoView({ block: "center", inline: "nearest" });
    element.click();
    return {
      url: location.href,
      selector: selector(args.selector),
      clicked: true,
    };
  }

  function typePage(args) {
    const element = selectedElement(args.selector);
    if (typeof args.text !== "string" || args.text.length > MAX_TYPED_CHARS) {
      throw new Error(`text must be a string no longer than ${MAX_TYPED_CHARS} characters`);
    }

    element.scrollIntoView({ block: "center", inline: "nearest" });
    element.focus();

    if (element.isContentEditable) {
      const next = args.clear === false
        ? String(element.textContent || "") + args.text
        : args.text;
      element.textContent = next;
    } else if ("value" in element) {
      const previous = args.clear === false ? String(element.value || "") : "";
      setNativeValue(element, previous + args.text);
    } else {
      throw new Error("selected element is not editable");
    }

    element.dispatchEvent(new InputEvent("input", {
      bubbles: true,
      inputType: "insertText",
      data: args.text,
    }));
    element.dispatchEvent(new Event("change", { bubbles: true }));

    return {
      url: location.href,
      selector: selector(args.selector),
      typed: true,
      length: args.text.length,
    };
  }

  function submitPage(args) {
    const element = selectedElement(args.selector);
    const form = element instanceof HTMLFormElement
      ? element
      : element.form || element.closest("form");

    if (form) {
      if (typeof form.requestSubmit === "function") {
        if (
          element !== form
          && (element instanceof HTMLButtonElement || element instanceof HTMLInputElement)
        ) {
          form.requestSubmit(element);
        } else {
          form.requestSubmit();
        }
      } else {
        form.submit();
      }
      return {
        url: location.href,
        selector: selector(args.selector),
        submitted: true,
      };
    }

    if (typeof element.click === "function") {
      element.click();
      return {
        url: location.href,
        selector: selector(args.selector),
        submitted: true,
        fallback: "click",
      };
    }

    throw new Error("selected element is not a form or submit control");
  }

  async function handle(message) {
    switch (message.action) {
      case "ping":
        return { ready: true, url: location.href };
      case "read":
        return readPage(message.args || {});
      case "elements":
        return elementsPage(message.args || {});
      case "extract":
        return extractPage(message.args || {});
      case "click":
        return clickPage(message.args || {});
      case "type":
        return typePage(message.args || {});
      case "submit":
        return submitPage(message.args || {});
      default:
        throw new Error("unsupported Zara page action");
    }
  }

  ext.runtime.onMessage.addListener((message, _sender, sendResponse) => {
    if (!message || message.source !== "zara") {
      return false;
    }
    Promise.resolve(handle(message))
      .then((result) => sendResponse(result))
      .catch((error) => sendResponse({
        __zara_error: error instanceof Error ? error.message : String(error),
      }));
    return true;
  });
})();
