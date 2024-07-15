onload = async () => {
    const resp = await fetch("/api" + location.pathname);
    const course = await resp.json();

    const courseName = document.querySelector("#course-name");
    courseName.insertAdjacentHTML(
        "beforeend",
        `<span>${course.__name}</span>`
    );

    const navpanel = document.querySelector("#problem-selector-navpanel");
    const description = document.querySelector("#problem-description");

    let lastSelectedProblem = 0;

    selectProblem = i => {
        navpanel.children[lastSelectedProblem].classList.remove("navbox-active");
        navpanel.children[i].classList.add("navbox-active");

        description.innerText = course.__problems[i].__description;

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
}
