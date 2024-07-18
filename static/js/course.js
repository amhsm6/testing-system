onload = async () => {
    const uploadInput = document.querySelector("#upload-file-input");
    uploadInput.value = null;

    const resp = await fetch("/api" + location.pathname);
    const course = await resp.json();

    const courseName = document.querySelector("#course-name");
    courseName.insertAdjacentHTML(
        "beforeend",
        `<h1>${course.__name}</h1>`
    );

    const navpanel = document.querySelector("#problem-selector-navpanel");
    const description = document.querySelector("#problem-description");

    let currSelectedProblemId;
    let lastSelectedProblem = 0;

    selectProblem = i => {
        currSelectedProblemId = course.__problems[i].__problemId;

        navpanel.children[lastSelectedProblem].classList.remove("navbox-active");
        navpanel.children[i].classList.add("navbox-active");

        description.innerHTML = course.__problems[i].__description;

        lastSelectedProblem = i;
    };

    for (let i = 0; i < course.__problems.length; i++) {
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

    const uploadButton = document.querySelector("#upload-solution");
    uploadButton.addEventListener("click", _ => {
        const ws = new WebSocket(`/api/submit/${currSelectedProblemId}`);

        ws.onopen = async () => {
            if (uploadInput.files.length != 1) { return; }

            const content = await uploadInput.files[0].text();
            ws.send(content);

            ws.send(JSON.stringify("Haskell"));

            uploadInput.value = null;
        };

        ws.onmessage = msg => {
            console.log(msg);
        };
    });
}
