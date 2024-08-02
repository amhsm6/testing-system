import { jwtDecode } from "jwt-decode"

onload = async () => {
    const link = document.querySelector("#header a[href = '/']");
    link.style.color = "gray";
    link.style.borderBottom = "2px solid gray";
    link.style.transition = "none";

    const resp = await fetch("/api/courses");
    const courses = await resp.json();

    const el = document.querySelector("#courses");

    courses.forEach(course => {
        el.insertAdjacentHTML(
            "beforeend",
            `
            <div class="course">
                <a href="/course/${course.id}">${course.name}</a>
            </div>
            `
        );
    });
}
