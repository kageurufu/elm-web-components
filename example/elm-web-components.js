class ElmComponent extends HTMLElement {
  init = null;
  app = null;

  constructor() {
    super();

    this.observer = new MutationObserver(this.onMutation.bind(this));
    this.observer.observe(this, {
      attributes: true,
      subtree: false,
      childList: false,
    });

    this.shadow = this.attachShadow({ mode: "open" });
  }

  connectedCallback() {
    this.app = this.init({
      node: this.shadow.appendChild(document.createElement("div")),
      flags: this.serializeAttributes(),
    });
  }

  serializeAttributes() {
    return Object.fromEntries(
      Array.from(this.attributes).map((v) => [v.name, v.value])
    );
  }

  onMutation(mutations, observer) {
    this.app.ports.updateFlags.send(JSON.stringify(this.serializeAttributes()));
  }
}

function initElmWebComponents(elm = window.Elm) {
  return Object.entries(elm).map(([name, component]) => {
    const elementName =
      "elm-" + name.replace(/([a-z0–9])([A-Z])/g, "$1-$2").toLowerCase();

    const newClass = {
      [name]: class extends ElmComponent {
        elementName = elementName;
        className = name;
        init = component.init;
      },
    }[name];

    customElements.define(elementName, newClass, {});
    return [elementName, newClass];
  });
}
