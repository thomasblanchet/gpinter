var notifications_allowed = false;

function onPermissionGranted() {
	notifications_allowed = true;
}

function onPermissionDenied() {
	notifications_allowed = false;
}

if (!Notify.needsPermission) {
    notifications_allowed = true;
} else if (Notify.isSupported()) {
    Notify.requestPermission(onPermissionGranted, onPermissionDenied);
}

function notify_user_success() {
	if (notifications_allowed) {
		var success_notification = new Notify('Interpolation completed', {
			body: 'You may now look at the results.',
		});
		success_notification.show();
	}
}

function notify_user_failure() {
	if (notifications_allowed) {
		var failure_notification = new Notify('Interpolation failed', {
			body: 'An error occurred while working with your data.',
		});
		failure_notification.show();
	}
}
