
(
s.quit;
s.boot;
s.options.numBuffers = 1024 * 256; // increase this if you need to load more samples
s.options.memSize = 8192 * 32; // increase this if you get "alloc failed" messages
s.options.maxNodes = 1024 * 32; // increase this if you are getting drop outs and the message "too many nodes"
)



// FLAT TONES
(
SynthDef(\singrain, { |freq = 440, j = 0, out|
	var sig;
	sig = SinOsc.ar(freq)*EnvGen.ar(Env(levels: [0, 0, 0.5, 0.5, 0], times: [0, 0.005, (0.015 + (0.025*j)), 0.005], curve: [0, 0, 0, 0]));
	Out.ar(out, sig ! 2);    // sig ! 2 is the same as [sig, sig]
}).add;
fork {
	(1..80).do { |j|
		var fname;
		(
			fname = "/Users/connorwessel/Music/SuperCollider Recordings/flat_pitch/flat_" ++ j ++ ".wav";
			f = Routine({
				s.record(duration:2.5, numChannels:1, path: fname);
				wait(0.1);
				(
					r = Routine({
						Synth(\singrain, [freq: 440, j: j]);

					});

				);

				r.play;

			});
			f.play;
			wait(3);
		)
	}
}
)

// PERCUSSIVE TONES
(
SynthDef(\singrain, { |freq = 440, j = 0, out|
	var sig;
	sig = SinOsc.ar(freq)*EnvGen.ar(Env(levels: [0, 0, 0.5, 0.5, 0], times: [0, 0.01, 0, (0.015 + (0.025*j))], curve: [0, -3, 0, -3]));
	Out.ar(out, sig ! 2);    // sig ! 2 is the same as [sig, sig]
}).add;
fork {
	(1..80).do { |j|
		var fname;
		(
			fname = "/Users/connorwessel/Music/SuperCollider Recordings/perc_pitch_durmatch/perc_" ++ j ++ ".wav";
			f = Routine({
				s.record(duration:2.5, numChannels:1, path: fname);
				wait(0.1);
				(
					r = Routine({
						Synth(\singrain, [freq: 440, j: j]);

					});

				);

				r.play;

			});
			f.play;
			wait(3);
		)
	}
}
)