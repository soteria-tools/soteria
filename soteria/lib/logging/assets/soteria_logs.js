function summaryToMsg(elem) {
  if (elem.tagName == "SUMMARY") {
    const div = document.createElement("div");
    div.className = "log-msg branch";
    for (let child of elem.childNodes) {
      div.appendChild(child);
    }
    elem.parentNode.replaceChild(div, elem);
  }
}

function flattenDirectDetails(element) {
  // Recurse first
  for (let child of element.children) {
    flattenDirectDetails(child);
  }

  // Will not be undefined iff there is a single <details> tag
  let detailChild = undefined;
  for (let child of element.children) {
    if (child.tagName == "DETAILS" && child.classList.contains("is-branch")) {
      if (detailChild == undefined) {
        detailChild = child;
      } else {
        detailChild = undefined;
        break;
      }
    }
  }

  if (detailChild == undefined) {
    return;
  }
  while (detailChild.firstChild) {
    summaryToMsg(detailChild.firstChild);
    element.insertBefore(detailChild.firstChild, detailChild);
  }

  element.removeChild(detailChild);
}

function observeSearch() {
  const searchInput = document.getElementById("search-input");
  const logMessages = document.querySelectorAll("div.log-msg");

  searchInput.addEventListener("input", () => {
    const searchRe = new RegExp(searchInput.value, "i");

    logMessages.forEach((message) => {
      const messageText = message.childNodes[0].textContent;
      message.style.display = searchRe.test(messageText) ? "block" : "none";
    });
  });
}

function keybindings() {
  const searchInput = document.getElementById("search-input");

  document.addEventListener("keydown", (event) => {
    if (event.target === searchInput) return;
    if (event.ctrlKey || event.metaKey) return;

    const details = document.querySelectorAll("details");
    console.log(event.key);
    if (event.key === "Escape") {
      searchInput.value = "";
      searchInput.dispatchEvent(new Event("input"));
      searchInput.focus();
      event.preventDefault();
      event.stopPropagation();
    } else if (event.key === "o") {
      details.forEach((detail) => {
        detail.open = true;
      });
    } else if (event.key === "c") {
      details.forEach((detail) => {
        detail.open = false;
      });
    } else if (event.key === "ArrowRight") {
      let active = document.activeElement;
      if (active.tagName === "SUMMARY") {
        active = active.parentNode;
      }
      if (active.open) {
        active.querySelectorAll("details").forEach((detail) => {
          detail.open = true;
        });
      } else {
        active.open = true;
      }
    } else if (event.key === "ArrowLeft") {
      let active = document.activeElement;
      if (active.tagName === "SUMMARY") {
        active = active.parentNode;
      }
      active.querySelectorAll("details").forEach((detail) => {
        if (detail !== active) {
          detail.open = false;
        }
      });
      active.open = false;
    }
  });
}

function populateFilter() {
  const filterBox = document.getElementById("filters");
  const filterClasses = Array.from(document.querySelectorAll(".log-msg"))
    .map((msg) => msg.className.split(" "))
    .flat();
  const uniqueClasses = [...new Set(filterClasses)];
  const prio = {
    SMT: 0,
    TRACE: 1,
    DEBUG: 2,
    INFO: 3,
    WARN: 4,
    ERROR: 5,
    APP: 6,
  };
  uniqueClasses.sort((l, r) => prio[r] - prio[l]);
  uniqueClasses.forEach((className) => {
    if (className === "log-msg") return;
    const option = document.createElement("input");
    option.type = "checkbox";
    option.name = className;
    option.id = className;
    option.checked = true;
    filterBox.appendChild(option);

    const label = document.createElement("label");
    label.htmlFor = className;
    label.textContent = className;
    label.className = className;
    filterBox.appendChild(label);

    option.addEventListener("change", () => {
      const checked = option.checked;
      const messages = document.querySelectorAll(`.log-msg.${className}`);
      messages.forEach((msg) => {
        msg.style.display = checked ? "block" : "none";
      });
    });
  });
}

// Start from the body
document.addEventListener("DOMContentLoaded", () => {
  // This is now useless since we show all branches as sections
  // even those which are not feasible.
  // Let's keep the code around for now, in case we want to change this.
  // flattenDirectDetails(document.body);
  observeSearch();
  keybindings();
  populateFilter();
});
