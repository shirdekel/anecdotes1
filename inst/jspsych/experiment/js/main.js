exitprompt = function() {
  return 'Are you sure you want to leave?';
};

// generate a random subject ID with 15 characters
var subject_id = jsPsych.randomization.randomID(15);

// pick a random condition for the subject at the start of the experiment
var align_condition = obj.conditions.align[jsPsych.randomization.sampleWithoutReplacement([0, 1], 1)[0]];
var anecdote_condition = obj.conditions.anecdote[jsPsych.randomization.sampleWithoutReplacement([0, 2, 3], 1)[0]];

// align_condition = "lowA";
// anecdote_condition = "statistics";

// record the condition assignment in the jsPsych data
jsPsych.data.addProperties({
  subject: subject_id,
  align_condition: align_condition,
  anecdote_condition: anecdote_condition
});

/* create timeline */
var timeline = [];

var test = {
  type: "survey-html-form",
  html: ""
};
//timeline.push(test);

/* pre-experiment pages (ethics, demogrpahics, and instructions)*/
// var timeline = timeline.concat(pre_exp);

/* instructions*/
var instructions = {
  type: "instructions",
  pages: obj.instructions[anecdote_condition],
  show_clickable_nav: true
};
timeline.push(instructions);

/* main task*/
if (anecdote_condition == "statistics") {
  var anecdote = "";
} else {
  var anecdote = "<p>" + obj.anecdote[align_condition] + "</p>";
}

var main_task = {
  type: 'survey-html-form3',
  html: anecdote + "<p>" + obj.projects[anecdote_condition] + "</p>"
};
timeline.push(main_task);

if (anecdote_condition !== "statistics") {
  var follow_up = {
    type: 'survey-html-form',
    html: obj.follow_up
  };
  timeline.push(follow_up);
}

// debrief
var debrief = {
  type: "instructions",
  pages: [
        '<img src="img/debrief1.png"></img>',
        '<img src="img/debrief2.png"></img>'
    ],
  show_clickable_nav: true
};
// timeline.push(debrief);

var debrief_block = {
    type: 'html-button-response',
    stimulus: obj.end_message,
    choices: ['End experiment']
};
timeline.push(debrief_block);

jsPsych.init({      
    timeline: timeline, 
    show_progress_bar: true,
    preload_images: obj.img_preload,
    on_finish: function() {
        onbeforeunload = "";

        saveData();
        //alert("It will take a few seconds to save your data after you press 'OK'. You will then be automatically redirected to Prolific.");
        //setTimeout("location.replace('https://app.prolific.co/submissions/complete?cc=8256C4AC');", 5000);
      }
});

// function saveData() {
//     var xhr = new XMLHttpRequest();
//     xhr.open('POST', 'SaveToDatabase.aspx'); // change 'write_data.php' to point to php script.
//     xhr.setRequestHeader('Content-Type', 'application/json');
//     xhr.onload = function () {
//         if (xhr.status == 200) {
//             var response = JSON.parse(xhr.responseText);
//             console.log(response.success);
//         }
//     };
//     xhr.send(jsPsych.data.get().json());
// }
