import { jwtDecode } from "jwt-decode"

onload = async () => {
    const emailInput = document.querySelector("#user-data-email");
    const passInput = document.querySelector("#user-data-pass");

    const cleanInputs = () => {
        emailInput.value = null;
        passInput.value = null;
    };

    cleanInputs();

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

    const loginButton = document.querySelector("#login-btn");
    loginButton.onclick = async _ => {
        if (!emailInput.validity.valid) {
            emailInput.style.border = "2px solid red";
            return;
        }
        if (!passInput.validity.valid) {
            passInput.style.border = "2px solid red";
            return;
        }

        const resp = await fetch("/api/login", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ email: emailInput.value, pass: passInput.value })
        });
        const data = await resp.json();

        cleanInputs();

        if (data.ok) {
            console.log(data.token);
        } else {
            if (data.err.errorCode == 1) {
                console.log("Wrong credentials")
            } else {
                console.error("error: unknown error code");
            }
        }
    };

    const regButton = document.querySelector("#reg-btn");
    regButton.onclick = async _ => {
        if (!emailInput.validity.valid || !passInput.validity.valid) { return; }

        /*const resp = await fetch("/api/register", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ email: emailInput.value, pass: passInput.value })
        });
        console.log(resp);
        console.log(await resp.json());

        cleanInputs();*/
    };
}
