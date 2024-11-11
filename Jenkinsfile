#!groovy
@Library('amf-jenkins-library') _

import groovy.transform.Field

def SLACK_CHANNEL = '#amf-jenkins'
def PRODUCT_NAME = "syaml"
def lastStage = ""
def color = '#FF8C00'
def headerFlavour = "WARNING"

pipeline {
    options {
        timeout(time: 30, unit: 'MINUTES')
        ansiColor('xterm')
    }
    agent {
        dockerfile {
            registryCredentialsId 'dockerhub-pro-credentials'
            registryCredentialsId 'github-salt'
            registryUrl 'https://ghcr.io'
        }
    }
    environment {
        NEXUS = credentials('exchange-nexus')
        NEXUSIQ = credentials('nexus-iq')
        GITHUB_ORG = 'aml-org'
        GITHUB_REPO = 'syaml'
        BUILD_NUMBER = "${env.BUILD_NUMBER}"
        BRANCH_NAME = "${env.BRANCH_NAME}"
        NPM_TOKEN = credentials('npm-mulesoft')
        CURRENT_VERSION = sh(script: "cat dependencies.properties | grep \"version\" | cut -d '=' -f 2", returnStdout: true)
    }
    stages {
        stage('Test') {
            steps {
                script {
                    lastStage = env.STAGE_NAME
                    sh 'sbt -mem 4096 -Dfile.encoding=UTF-8 clean coverage test coverageAggregate'
                }
            }
        }
        stage('Coverage') {
            when {
                anyOf {
                    branch 'master'
                }
            }
            steps {
                withCredentials([[$class: 'UsernamePasswordMultiBinding', credentialsId: 'sf-sonarqube-official', passwordVariable: 'SONAR_SERVER_TOKEN', usernameVariable: 'SONAR_SERVER_URL']]) {
                    script {
                        lastStage = env.STAGE_NAME
                        sh 'sbt -Dsonar.host.url=${SONAR_SERVER_URL} -Dsonar.login=${SONAR_SERVER_TOKEN} sonarScan'
                    }
                }
            }
        }
        stage('Publish') {
            when {
                anyOf {
                    branch 'master'
                }
            }
            steps {
                script {
                    lastStage = env.STAGE_NAME
                    sh '''
                               echo "about to publish in sbt"
                               sbt syamlJS/publish
                               sbt syamlJVM/publish
                               echo "sbt publishing successful"
                           '''
                }
            }
        }
        stage('Tag version') {
            when {
                anyOf {
                    branch 'master'
                }
            }
            steps {
                withCredentials([[$class: 'UsernamePasswordMultiBinding', credentialsId: 'github-salt', passwordVariable: 'GITHUB_PASS', usernameVariable: 'GITHUB_USER']]) {
                    script {
                        lastStage = env.STAGE_NAME
                        def version = sbtArtifactVersion("syamlJVM")
                        tagCommitToGithub(version)
                    }
                }
            }
        }
    }
    post {
        unsuccessful {
            failureSlackNotify(lastStage, SLACK_CHANNEL, PRODUCT_NAME)
        }
        success {
            successSlackNotify(SLACK_CHANNEL, PRODUCT_NAME)
        }
    }
}
