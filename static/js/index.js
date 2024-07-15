onload = async () => {
    const link = document.querySelector("#header a[href = '/']");
    link.style.setProperty("color", "gray");
    link.style.setProperty("border-bottom", "2px solid gray");
    link.style.setProperty("transition", "none");

    const resp = await fetch("/api/courses");
    const courses = await resp.json();

    const el = document.querySelector("#courses");

    courses.forEach(course => {
        el.insertAdjacentHTML(
            "beforeend",
            `
            <div class="course">
                <a href="/course/${course.__courseId}">${course.__name}</a>
            </div>
            `
        );
    });
}
