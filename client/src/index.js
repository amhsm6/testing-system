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

    const renderUserData = () => {
        document.querySelector("#user-logged-in").style.display = "";
        document.querySelector("#user-not-logged-in").style.display = "none";
    };

    const emailInput = document.querySelector("#user-data-email");
    const passInput = document.querySelector("#user-data-pass");

    const cleanInputs = () => {
        emailInput.value = null;
        emailInput.style.border = "2px solid black";

        passInput.value = null;
        passInput.style.border = "2px solid black";
    };

    const renderLoginForm = () => {
        cleanInputs();

        document.querySelector("#user-logged-in").style.display = "none";
        document.querySelector("#user-not-logged-in").style.display = "";
    };

    const token = localStorage.getItem("token");
    if (token) { renderUserData(); } else { renderLoginForm(); }

    const logoutButton = document.querySelector("#logout-btn");
    logoutButton.onclick = _ => {
        localStorage.removeItem("token");
        renderLoginForm();
    };

    const loginErrors = document.querySelector("#login-errors");

    emailInput.oninput = _ => loginErrors.replaceChildren();
    passInput.oninput = _ => loginErrors.replaceChildren();

    const loginButton = document.querySelector("#login-btn");
    loginButton.onclick = async _ => {
        let invalid = false;

        if (!emailInput.validity.valid) {
            emailInput.style.border = "2px solid red";
            invalid = true;
        }

        if (!passInput.validity.valid) {
            passInput.style.border = "2px solid red";
            invalid = true;
        }

        if (invalid) { return; }

        const resp = await fetch("/api/login", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ email: emailInput.value, pass: passInput.value })
        });
        const data = await resp.json();

        cleanInputs();

        if (data.ok) {
            localStorage.setItem("token", data.token);
            renderUserData();
        } else {
            if (data.err.errorCode == 1) {
                loginErrors.insertAdjacentHTML(
                    "beforeend",
                    `<span>Wrong Credentials</span>`
                );
            } else {
                loginErrors.insertAdjacentHTML(
                    "beforeend",
                    `<span>Unknown Error</span>`
                );
            }
        }
    };

    const regButton = document.querySelector("#reg-btn");
    regButton.onclick = async _ => {
        /*let invalid = false;

        if (!emailInput.validity.valid) {
            emailInput.style.border = "2px solid red";
            invalid = true;
        }

        if (!passInput.validity.valid) {
            passInput.style.border = "2px solid red";
            invalid = true;
        }

        if (invalid) { return; }

        const resp = await fetch("/api/register", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ email: emailInput.value, pass: passInput.value })
        });
        const data = await resp.json();

        cleanInputs();

        if (data.ok) {
            localStorage.setItem("token", data.token);
            renderUserData();
        } else {
            if (data.err.errorCode == 1) {
                loginErrors.insertAdjacentHTML(
                    "beforeend",
                    `<span>Wrong Credentials</span>`
                );
            } else {
                loginErrors.insertAdjacentHTML(
                    "beforeend",
                    `<span>Unknown Error</span>`
                );
            }
        }*/
    };
}
