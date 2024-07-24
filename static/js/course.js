const knownExtensions = {
    "hs": "Haskell",
    "c": "C",
    "cpp": "Cpp",
    "py": "Python"
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
    }

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
    const closeButton = document.querySelector("#close-results");

    onkeydown = e => {
        if (e.key == "Escape") {
            uploadResults.style.display = "none";
            uploadLog.replaceChildren();
        }
    }

    closeButton.onclick = () => {
        uploadResults.style.display = "none";
        uploadLog.replaceChildren();
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

                console.log(data);
            }
        };
    };
}
