const knownExtensions = {
    "hs": "Haskell",
    "c": "C",
    "cpp": "Cpp",
    "py": "Python"
};

const errorString = {
    1: "Error: Such problem does not exist",
    2: "Error: Testing programs in that language is not supported",
    3: "Error: Something went wrong during testing"
};

onload = async () => {
    const uploadInput = document.querySelector("#upload-file-input");
    uploadInput.value = null;

    const languageSelector = document.querySelector("#language-selector");
    languageSelector.value = "";

    uploadInput.oninput = () => {
        if (uploadInput.files.length != 1) { return; }

        const filename = uploadInput.files[0].name;
        const ext = filename.split('.').pop();
        languageSelector.value = knownExtensions[ext] || "";
    };

    const resp = await fetch("/api" + location.pathname);
    const course = await resp.json();

    const courseName = document.querySelector("#course-name");
    courseName.insertAdjacentHTML(
        "beforeend",
        `<h1>${course.name}</h1>`
    );

    const navpanel = document.querySelector("#problem-selector-navpanel");
    const description = document.querySelector("#problem-description");

    let currSelectedProblemId;
    let lastSelectedProblem = 0;

    selectProblem = i => {
        if (!course.problems[i]) { return; }

        currSelectedProblemId = course.problems[i].id;

        navpanel.children[lastSelectedProblem].classList.remove("navbox-active");
        navpanel.children[i].classList.add("navbox-active");

        description.innerHTML = course.problems[i].description;
        uploadInput.value = null;
        languageSelector.value = "";

        lastSelectedProblem = i;
    };

    for (let i = 0; i < course.problems.length; i++) {
        navpanel.insertAdjacentHTML(
            "beforeend",
            `
            <div class="navbox">
                <button onclick="selectProblem(${i})">&numero;${i + 1}</button>
            </div>
            `
        );
    }

    selectProblem(0);

    const uploadResults = document.querySelector("#upload-results");
    const uploadLog = document.querySelector("#upload-log");
    const problemStatus = document.querySelector("#problem-status");
    const errorDescription = document.querySelector("#error-description");
    const closeButton = document.querySelector("#close-results");

    const clearUploadResults = () => {
        uploadResults.style.display = "none";
        uploadLog.replaceChildren();
        errorDescription.replaceChildren();
    };

    onkeydown = e => {
        if (e.key == "Escape") {
            clearUploadResults();
        }
    };

    closeButton.onclick = () => {
        clearUploadResults();
    };

    const openText = text => {
        const wnd = open();
        wnd.document.write(text)
        return wnd;
    };

    displayInput = data => {
        const wnd = openText(data);
        wnd.document.title = "Failed Test Input";
    };

    displayOutput = data => {
        const wnd = openText(data);
        wnd.document.title = "Expected Output";
    };

    const uploadButton = document.querySelector("#upload-solution");
    uploadButton.onclick = () => {
        const ws = new WebSocket(`/api/submit/${currSelectedProblemId}`);

        ws.onopen = async () => {
            if (uploadInput.files.length != 1) { return; }
            if (languageSelector.value === "") { return; }

            const content = await uploadInput.files[0].text();
            ws.send(JSON.stringify({
                source: content,
                lang: languageSelector.value
            }));

            uploadInput.value = null;
            languageSelector.value = "";

            problemStatus.innerHTML = "<span>PROCESSING</span>";
            problemStatus.style.backgroundColor = "yellow";
            uploadResults.style.display = "";

            scroll(0, 0);
        };

        ws.onmessage = msg => {
            const data = JSON.parse(msg.data);

            if (data.ok) {
                if (data.logs) {
                    uploadLog.replaceChildren();
                    data.logs.forEach(log => {
                        uploadLog.insertAdjacentHTML(
                            "beforeend",
                            `<span>${log}</span>`
                        );
                    });
                } else {
                    problemStatus.innerHTML = "<span>PASSED</span>";
                    problemStatus.style.backgroundColor = "green";
                    closeButton.style.display = "";
                }
            } else {
                problemStatus.innerHTML = "<span>FAILED</span>";
                problemStatus.style.backgroundColor = "red";
                closeButton.style.display = "";

                if (errorString[data.err.errorCode]) {
                    errorDescription.insertAdjacentHTML(
                        "beforeend",
                        `<span>${errorString[data.err.errorCode]}</span>`
                    );
                } else if (data.err.errorCode == 5 || data.err.errorCode == 6) {
                    errorDescription.insertAdjacentHTML(
                        "beforeend",
                        `
                        <a class="input" onclick="displayInput('${data.err.errorData.input}')">Failed Test Input</a>
                        <a class="output" onclick="displayOutput('${data.err.errorData.output}')">Expected Output</a>
                        `
                    );
                } else {
                    console.error("error: unknown error code");
                }
            }
        };
    };
}
