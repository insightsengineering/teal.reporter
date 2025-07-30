$(document).on("shiny:busy", function () {
  $(".teal-reporter-busy-disable").prop("disabled", true);
});
$(document).on("shiny:idle", function () {
  $(".teal-reporter-busy-disable").prop("disabled", false);
});
