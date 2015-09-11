// Negtracker Ipad
// Overview: (i) Helper Functions (ii) Parameters (iii) Control Flow

// ---------------- HELPER FUNCTIONS ------------------

//call the maker getter to get the cond variable
//make sure you change the file name for each new study!!  
//(Stephan Meylan wrote this program which lets you specify a between-subjects variable and the # of participants you want in each condition.)
var xmlHttp = null;
xmlHttp = new XMLHttpRequest();
xmlHttp.open("GET", "https://langcog.stanford.edu/cgi-bin/subject_equalizer/maker_getter.php?conds=0,50;1,50&filename=aen_negpad_adults2_test2", false);
xmlHttp.send(null);
var cond = xmlHttp.responseText;

var testCondition;
if (cond == 0) {
	testCondition = "noContext";
} else {
	testCondition = "context";
}

// show slide function
function showSlide(id) {
	$(".slide").hide(); //jquery - all elements with class of slide - hide
	$("#" + id).show(); //jquery - element with given id - show
}

//array shuffle function
function shuffle(array) {
	for (var i = array.length - 1; i > 0; i--) {
		var j = Math.floor(Math.random() * (i + 1));
		var temp = array[i];
		array[i] = array[j];
		array[j] = temp;
	}
	return array;
}

//preload images: 
var myimages = new Array();

function preloading() {
	for (x = 0; x < preloading.arguments.length; x++) {
		myimages[x] = new Image();
		myimages[x].src = preloading.arguments[x];
	}
}

function loadAudio(uri) {
	var audio = new Audio();
	audio.src = uri;
	return audio;
}

preloading("images/Abby.png", "images/apple.png", "images/banana.png", "images/BigBird.png", "images/Bert.png", "images/boat.png", "images/bowl.png", "images/bus.png", "images/car.png", "images/cat.png", "images/cookie.png", "images/cow.png", "images/dog.png", "images/Elmo.png", "images/empty.png", "images/Ernie.png", "images/fork.png", "images/grover.png", "images/horse.png", "images/list.txt", "images/orange.png", "images/plate.png", "images/Rosita.png", "images/spoon.png", "images/stanford.png", "images/starOff.png", "images/starOn.png", "images/truck.png", "images/Zoe.png");

//For training:
function createDot(dotx, doty, i) {
	var dots = [1, 2, 3, 4, 5];

	var dot = document.createElement("img");
	dot.setAttribute("class", "dot");
	dot.id = "dot_" + dots[i];
	dot.src = "dots/dot_" + dots[i] + ".jpg";

	var x = Math.floor(Math.random() * 950);
	var y = Math.floor(Math.random() * 550);

	var invalid = "true";
	//make sure dots do not overlap
	while (true) {
		invalid = "true";
		for (j = 0; j < dotx.length; j++) {
			if (Math.abs(dotx[j] - x) + Math.abs(doty[j] - y) < 200) {
				var invalid = "false";
				break;
			}
		}
		if (invalid === "true") {
			dotx.push(x);
			doty.push(y);
			break;
		}
		x = Math.floor(Math.random() * 400);
		y = Math.floor(Math.random() * 400);
	}

	dot.setAttribute("style", "position:absolute;left:" + x + "px;top:" + y + "px;");

	training.appendChild(dot);
}

///BUILD SLIDER
$(function() { //loads the slider 
	$("#slider").slider({
		min: 1,
		max: 7,
		value: 4,
		start: function(event, ui) {}
	})
})

//Make slider labels
// Get the number values
var vals = 6;

//Labels
var labs = ["Very Bad", "Bad", "Somewhat Bad", "Neutral", "Somewhat Good", "Good", "Very Good"];

// Space out values
for (var i = 0; i <= vals; i++) {
	var el = $('<label>' + labs[i] + '</label>').css('left', ((i / vals * 100) - 2) + '%');
	$("#slider").append(el);
}


// ---------------- PARAMETERS ------------------
var subjectID = turk.workerId;
var debugMode = false;

// testword for when running audio studies on turk
// var testWord = "purple"

var characters = ["Abby", "BigBird", "Bert", "Elmo", "Ernie", "Grover", "Rosita", "Zoe"];

//-----PRACTICE VARIABLES-----
var practiceCounter = 0;
//Total number of practice trials:
var practiceNumber = 0;

//Practice items (None for adults)
var practiceItems = [""];

//Trial conditions: 
//Does elmo have the target item or not?
//Is the sentence positive or negative?
var practiceConds = [
	[1, "positive"]
]

//-----EXPERIMENT VARIABLES-----
var trialCounter = 0;
//total number of test trials:
var number = 16;

//The first item is the actual trial item, the second item is the alternative (for "something" trials)
var trialItems = shuffle([
	["apple", "cat"],
	["banana", "dog"],
	["boat", "orange"],
	["bowl", "truck"],
	["bus", "cookie"],
	["car", "plate"],
	["cat", "apple"],
	["cookie", "bus"],
	["cow", "spoon"],
	["dog", "banana"],
	["fork", "horse"],
	["horse", "fork"],
	["orange", "boat"],
	["plate", "car"],
	["spoon", "cow"],
	["truck", "bowl"]
]);

//Trial conditions: 
//Does elmo have the target item or not?
//Is the sentence positive or negative?
//If Elmo doesn't have target items, does he have a different item ("something" trials)
//What type of negative sentence

var trialConds = shuffle([
	[1, "positive", "NA", "NA"], //True positive
	[1, "positive", "NA", "NA"], //True positive
	[1, "positive", "NA", "NA"], //True positive
	[1, "positive", "NA", "NA"], //True positive
	[1, "negative", "NA", "has no"], //False negative, "no"
	[1, "negative", "NA", "doesn't have"], //False negative, "doesn't"
	// [1, "negative", "NA", "has no"], //False negative, "no"
	// [1, "negative", "NA", "doesn't have"], //False negative, "doesn't"
	[0, "positive", "nothing", "NA"], //False positive
	// [0, "positive", "nothing", "NA"], //False positive
	// [0, "positive", "something", "NA"], //False positive
	[0, "positive", "something", "NA"], //False positive
	[0, "negative", "nothing", "has no"], //True negative, "no", nonexistence
	[0, "negative", "nothing", "doesn't have"], //True negative, "doesn't", nonexistence
	[0, "negative", "something", "has no"], //True negative, "no", "alternative"
	[0, "negative", "something", "doesn't have"], //True negative, "doesn't", "alternative"
	[0, "negative", "nothing", "has no"], //True negative, "no", nonexistence
	[0, "negative", "nothing", "doesn't have"], //True negative, "doesn't", nonexistence
	[0, "negative", "something", "has no"], //True negative, "no", "alternative"
	[0, "negative", "something", "doesn't have"] //True negative, "doesn't", "alternative"
]);

// ---------------- MAIN EXPERIMENT ------------------
//Show the first instructions slide (this is where you enter pswd and subid on ipad study)
showSlide("instructions");

//The button is disabled until all of the images are preloaded
//Button is also disabled if turk is in preview mode
$("#startButton").attr("disabled", true);
if (turk.previewMode != true) {
	$("#pleaseWait").html("Please wait...");
	$(window).load(function() {
		$("#startButton").attr("disabled", false);
		$("#pleaseWait").html("");
	})
}

//Start the experiment
var experiment = {

	gender: [],
	age: "",
	nativeLanguage: "",
	comments: "",

	//Build the progress bar
	initStars: function(starNumber) {
		$("#progress").html("");
		for (i = 0; i < starNumber; i++) {
			$("#progress").append("<img class='star' src = 'images/starOff.png' id='star" + i + "'/>");
		}
	},

	practice_context: function() {
		experiment.context(practiceItems, practiceConds, practiceCounter, practiceNumber, "Practice trial");
	},

	trial_context: function() {
		experiment.context(trialItems, trialConds, trialCounter, number, "Test trial");
	},

	//sound in child version only
	// soundCheck: function() {
	// 	//make sure that the computer's sound is on
	// 	showSlide("soundTest");
	// 	var keyPressHandler = function(event) {
	// 		var keyCode = event.which;
	// 		if (keyCode == 13) {
	// 			if (experiment.checkSoundTest()) {
	// 				$(document).off("keydown");
	// 			}
	// 		} else {
	// 			$("#soundtest_input").focus();
	// 		}
	// 	};
	// 	$(document).on("keydown", keyPressHandler);
	// },

	// playTest: function() {
	// 	$("#test_player")[0].play();
	// },

	// checkSoundTest: function() {
	// 	if ($("#soundtest_input").val().toLowerCase() == testWord) {
	// 		experiment.practice_start();
	// 		return true;
	// 	} else {
	// 		$("#soundtest_wrong").show();
	// 		return false;
	// 	}
	// },

	blank: function(count, max, trialType) {
		showSlide("blankSlide");
		// 	//reset startTime & endTime
		// startTime = ""
		// endTime = ""
		//Check if the practice trials are completed, and go to experiment.start if they are
		if (count === max) {
			if (trialType === "Practice trial") {
				return experiment.start();
			} else {
				return experiment.background();
			}
		} else {
			if (trialType === "Practice trial") {
				setTimeout(function() {
					experiment.next(practiceItems, practiceConds, practiceCounter, practiceNumber, "Practice trial");
				}, 500)
			} else {
				setTimeout(function() {
					experiment.next(trialItems, trialConds, trialCounter, number, "Test trial");
				}, 500)
			}

		}

	},

	practice_start: function() {
		showSlide("trainingInstructions");

		//decrement the condition counter once participant has made it through practice trials
		var xmlHttp = null;
		xmlHttp = new XMLHttpRequest();
		xmlHttp.open("GET", "https://langcog.stanford.edu/cgi-bin/subject_equalizer/decrementer.php?filename=aen_negpad_adults2_test&to_decrement=" + cond, false);
		xmlHttp.send(null);
	},

	start: function() {
		//Show the third instructions slide, which allows you to start the game.
		showSlide("instructions3");
	},

	//The actual trials
	next: function(items, trialCond, counter, max, trialType) {

		showSlide("stage");
		$("#listenDiv").hide();
		$("#nextTrialButton").hide();
		$('.ui-slider-handle').hide();

		//show pics
		$("#pic1").show();
		$("#pic2").show();
		$("#pic3").show();
		$("#pic4").show();

		//Determine which context pictures will be used.  
		if (testCondition == "noContext") {
			var contextPictures = "empty";
		} else if (testCondition == "context") {
			var contextPictures = items[counter][0];
		}

		//Should the target have an item or an empty table?
		if (trialCond[counter][0]) {
			var trialPicture = items[counter][0];
		} else {
			if (trialCond[counter][2] == "nothing") {
				var trialPicture = "empty";
			} else {
				var trialPicture = items[counter][1];
			}
		}

		//create characters
		characters = shuffle(characters);

		var charName1 = "images/" + characters[0] + ".png";
		var charName2 = "images/" + characters[1] + ".png";
		var charName3 = "images/" + characters[2] + ".png";
		var charNameTarget = "images/" + characters[3] + ".png";

		var targetChar = characters[3];


		//create sentence, which will appear later
		if (items[counter][0][0] == "a" | items[counter][0][0] == "e" | items[counter][0][0] == "i" | items[counter][0][0] == "o" | items[counter][0][0] == "u") {
			var det = " an ";
		} else {
			var det = " a ";
		}

		if (trialCond[counter][1] == "positive") {
			var sentence_text = targetChar + " has" + det + items[counter][0] + ".";
		} else if (trialCond[counter][1] == "negative") {
			if (trialCond[counter][3] == "has no") {
				var sentence_text = targetChar + " " + trialCond[counter][3] + " " + items[counter][0] + ".";
			} else {
				var sentence_text = targetChar + " " + trialCond[counter][3] + det + items[counter][0] + ".";
			}
		}

		$("#sentenceText").html(sentence_text);

		//shuffle the order that pictures appear in the array
		var picOrder = shuffle(["context1", "context2", "context3", "target"]);
		$("#pic1").attr("id", picOrder[0]);
		$("#pic2").attr("id", picOrder[1]);
		$("#pic3").attr("id", picOrder[2]);
		$("#pic4").attr("id", picOrder[3]);

		//Place the images
		var context1 = "images/" + contextPictures + ".png";
		$("#context1").attr("src", context1);

		var context2 = "images/" + contextPictures + ".png";
		$("#context2").attr("src", context2);

		var context3 = "images/" + contextPictures + ".png";
		$("#context3").attr("src", context3);

		$("#target").attr("src", "images/" + trialPicture + ".png");

		// //This is a counter that tells us when all context images have been clicked
		// var ccounter = 0;
		// var ctotal = 4;

		// //When images are clicked, replace with new images and play the "bloop" noise
		// $('.pic').one('click', function(event) {
		// 	var cpicID = $(event.currentTarget).attr('id');

		// 	// $("#bloop_player")[0].play();
		// 	if (cpicID == "context1") {
		$("#context1").css("background-image", 'url(' + charName1 + ')');
		// 	} else if (cpicID == "context2") {
		$("#context2").css("background-image", 'url(' + charName2 + ')');
		// 	} else if (cpicID == "context3") {
		$("#context3").css("background-image", 'url(' + charName3 + ')');
		// 	} else if (cpicID == "target") {
		$("#target").css("background-image", 'url(' + charNameTarget + ')');
		// 	}

		// 	ccounter++;

		// 	if (ccounter === ctotal) {
		setTimeout(function() {

				//show listen button
				$("#listenDiv").show();
				// $("#listenText").show();

				// //When button is clickped, play sentence
				// $('#listenButton').one('click', function() {

				// 	//load sound
				// 	$("#sound_player")[0].load();

				// 	//change listen button image
				// 	$('#listenButton').attr("src", "images/listening.jpg");
				// 	$("#listenText").text("Listen! Then click Elmo's Plate!");

				// 	$("#sound_player")[0].play();

				// 	$('#sound_player').one('ended', function() {
				// 		$(".value").show();
				// 		$("#slider").show();
				// 		$(function() { //loads the slider when the sentence has ended.
				// 			$("#slider").slider({
				// 				min: 1,
				// 				max: 7,
				// 				value: 1,
				// 				start: function(event, ui) {
				// 					$("#nextTrialButton").show();
				// 				}
				// 			});
				// 		});
				// 	});

				//red box appears around target
				$("#target").attr('class', 'selectedPic');

				$("#slider").on("slidestart", function(event, ui) {
					$("#nextTrialButton").show();
					$('.ui-slider-handle').show();
				});

				//After submit button is pressed
				$("#nextTrialButton").one('click', function() {

					//collect data
					var trialNum = counter + 1;

					var response = $('#slider').slider("option", "value");

					if ((trialCond[counter][0] & trialCond[counter][1] == "positive") | (!trialCond[counter][0] & trialCond[counter][1] == "negative")) {
						var truthVal = "True";
					} else {
						var truthVal = "False";
					}

					//this is: subid, context condition, trial number, item, sentence type, negation type, negation frame, truth value, response(what the participant chose)
					var result_string = subjectID + "," + testCondition + "," + trialNum + "," + items[counter][0] + "," + trialCond[counter][1] + "," + trialCond[counter][2] + "," + trialCond[counter][3] + "," + truthVal + "," + response + "\n";

					//no data collection for demo
					// if (debugMode) {
					// 	alert(result_string); // debug mode 
					// } else {
					// 	$.post("https://langcog.stanford.edu/cgi-bin/AEN2/NEGPAD_verification/negpad_process.php", {
					// 		postresult_string: result_string
					// 	})
					// }

					//update progress bar
					$("#star" + (counter)).attr("src", "images/starOn.png");

					//update counter
					counter++;
					if (trialType == "Practice trial") { //increment the appropriate counter
						practiceCounter++;
					} else {
						trialCounter++;
					}

					//clear everything
					setTimeout(function() {

						$("#listenDiv").hide();
						$("nextTrialButton").hide();
						$("#sentenceText").html("");
						$("#target").attr('class', 'pic');
						$("#context1").css("background-image", 'none');
						$("#context2").css("background-image", 'none');
						$("#context3").css("background-image", 'none');
						$("#target").css("background-image", 'none');
						$(".pic").hide();
						// $("#listenText").text("(Click me!)");
						// $("#listenButton").attr("src", "images/listen.jpg");

						//reset pic ids
						$("#" + picOrder[0]).attr("id", "pic1");
						$("#" + picOrder[1]).attr("id", "pic2");
						$("#" + picOrder[2]).attr("id", "pic3");
						$("#" + picOrder[3]).attr("id", "pic4");

						//next trial
						experiment.blank(counter, max, trialType);
					})
				})
			}, 0)
			// }
			// })
	},

	//For turk only
	background: function() {

		//undo spacebar disable
		window.onkeydown = function(e) {}

		$("#gender").trigger("reset");
		$("#age").trigger("reset");
		$("#language").trigger("reset");
		$("#aboutQ").trigger("reset");
		$("#commentQ").trigger("reset");
		showSlide("askInfo");

		$("#endButton").click(function() {
			var gen = $("input:radio[name=genderButton]:checked").val();
			var ag = $("#ageRange").val();
			var lan = $("#nativeLanguage").val();
			var comm = $("#commentQ").val();

			if (gen == "" | ag == "" | lan == "") {
				alert("Please answer all of the questions");
			} else {

				//No data collection for demo experiment.
				// $.post("https://langcog.stanford.edu/cgi-bin/AEN2/NEGPAD_verification/negpad_subject.php", {
				// 	postresult_string: result_string
				// })

				experiment.gender = gen
				experiment.age = ag
				experiment.nativeLanguage = lan
				experiment.comments = comm

				experiment.end();
			}
		})
	},

	end: function() {
		showSlide("finished");

		//no data collection for demo experiment
		// setTimeout(function() {
		// 	turk.submit(experiment, true);
		// }, 1500);
	}
}