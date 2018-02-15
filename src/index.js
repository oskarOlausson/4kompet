import './main.css';
import { Main } from './Main.elm';
//import registerServiceWorker from './registerServiceWorker';


var app = Main.embed(document.getElementById('root'));



var config = {
  apiKey: "AIzaSyDcmS6CjRvR5UpBus2EBV5U2Hhx5AH2Ewc",
  authDomain: "kompet-51128.firebaseapp.com",
  databaseURL: "https://kompet-51128.firebaseio.com",
  projectId: "kompet-51128",
  storageBucket: "",
  messagingSenderId: "956490581748"
};
firebase.initializeApp(config);

//registerServiceWorker();


var dbRef = firebase.database().ref().child("songs");

dbRef.on('child_added', snap => {
        console.log("Something was added: " + snap);

        var asJson = snap.toJSON();

        //we attach the id to the object so we can update them and remove them and stuff
        //we call it id since key is being used for the songs key (ex key = "A-major")
        asJson.id = snap.key;

        app.ports.recieveNewSong.send(asJson);
    }
);


dbRef.on('child_changed', snap => {
        console.log("Something changed: " + snap);

        var asJson = snap.toJSON();

        //we attach the id to the object so we can update them and remove them and stuff
        //we call it id since key is being used for the songs key (ex key = "A-major")
        asJson.id = snap.key;

        app.ports.recieveUpdatedSong.send(asJson);
    }
);

dbRef.on('child_removed', snap => {
        console.log("Something was removed: " + snap);
        app.ports.recieveRemoveQuery.send(snap.key);
    }
);

app.ports.amILoggedIn.subscribe(function() {
    if (firebase.auth().currentUser) app.ports.signedIn.send(true);
    else app.ports.signedIn.send(false);
})


app.ports.newSong.subscribe(function(song) {
    var ref  = dbRef.push();
    song.id = ref.key;
    app.ports.setId.send(ref.key);

    ref.set(song).catch(function(error) {
        app.ports.invalidPassword.send(error.message);
    });
});

app.ports.updateSong.subscribe(function(song) {
    console.log("updating" + song);
    var ref = dbRef.child(song.id);


    ref.set(song).catch(function(error) {
        app.ports.errorMsg.send(error.message);
    });
});

app.ports.removeSong.subscribe(function(songId) {

    var ref = dbRef.child(songId);


    ref.remove();
});

app.ports.signIn.subscribe(function(psw) {
    var userName = "kullenoskar@gmail.com"

    firebase.auth().signInWithEmailAndPassword(userName, psw).catch(function(error) {
        app.ports.signedIn.send(false);
        app.ports.errorMsg.send("Ogiltigt lösenord");
    });


    firebase.auth().onAuthStateChanged(function(user) {
        if (user) {
          // User is signed in.
            app.ports.signedIn.send(true);
        } else {
            app.ports.signedIn.send(false);
        }
    });
});
